;;; GNU Guix --- Functional package management for GNU
;;; Copyright Â© 2021 raid5atemyhomework <raid5atemyhomework@protonmail.com>
;;;
;;; This file is part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (abbe services file-systems)
  #:use-module (gnu packages file-systems)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services linux)
  #:use-module (gnu services mcron)
  #:use-module (gnu services shepherd)
  #:use-module ((gnu system file-systems) #:select (file-system-mount-point))
  #:use-module (gnu system mapped-devices)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (guix packages)
  #:use-module (guix records)
  #:use-module (srfi srfi-1)
  #:export (zfs-service-type

            zfs-configuration
            zfs-configuration?
            zfs-configuration-kernel
            zfs-configuration-base-zfs
            zfs-configuration-base-zfs-auto-snapshot
            zfs-configuration-dependencies
            zfs-configuration-auto-mount?
            zfs-configuration-auto-scrub
            zfs-configuration-auto-snapshot?
            zfs-configuration-auto-snapshot-keep

            %zfs-zvol-dependency))

(define (file-system->shepherd-service-name file-system)
  "Return the symbol that denotes the service mounting and unmounting
FILE-SYSTEM."
  (symbol-append 'file-system-
                 (string->symbol (file-system-mount-point file-system))))

(define (mapped-device->shepherd-service-name md)
  "Return the symbol that denotes the shepherd service of MD, a <mapped-device>."
  (symbol-append 'device-mapping-
                 (string->symbol (string-join
                                  (mapped-device-targets md) "-"))))

(define dependency->shepherd-service-name
  (match-lambda
    ((? mapped-device? md)
     (mapped-device->shepherd-service-name md))
    ((? file-system? fs)
     (file-system->shepherd-service-name fs))))

(define-record-type* <zfs-configuration>
  zfs-configuration
  make-zfs-configuration
  zfs-configuration?

  ;; linux-libre kernel you want to compile the base-zfs module for.
  (kernel                     zfs-configuration-kernel)

  ;; the OpenZFS package that will be modified to compile for the
  ;; given kernel.
  ;; Because it is modified and not the actual package that is used,
  ;; we prepend the name 'base-'.
  (base-zfs                   zfs-configuration-base-zfs
                              (default zfs))

  ;; the zfs-auto-snapshot package that will be modified to compile
  ;; for the given kernel.
  ;; Because it is modified and not the actual package that is used,
  ;; we prepend the name 'base-'.
  (base-zfs-auto-snapshot     zfs-configuration-base-zfs-auto-snapshot
                              (default zfs-auto-snapshot))

  ;; list of <mapped-device> or <file-system> objects that must be
  ;; opened/mounted before we import any ZFS pools.
  (dependencies               zfs-configuration-dependencies
                              (default '()))

  ;; #t to mount all mountable datasets by default.
  ;; #f if not mounting.
  ;; #t is the expected behavior on other operating systems, the
  ;; #f is only supported for "rescue" operating systems where
  ;; the user wants lower-level control of when to mount.
  (auto-mount?                zfs-configuration-auto-mount?
                              (default #t))

  ;; 'weekly for weekly scrubbing, 'monthly for monthly scrubbing, an
  ;; mcron time specification that can be given to `job`, or #f to
  ;; disable.
  (auto-scrub                 zfs-configuration-auto-scrub
                              (default 'weekly))

  ;; #t to auto-snapshot by default (and `com.sun:auto-snapshot=false`
  ;; disables auto-snapshot per dataset), #f to not auto-snapshot
  ;; by default (and `com.sun:auto-snapshot=true` enables auto-snapshot
  ;; per dataset).
  (auto-snapshot?             zfs-configuration-auto-snapshot?
                              (default #t))

  ;; association list of symbol-number pairs to indicate the number
  ;; of automatic snapshots to keep for each of 'frequent, 'hourly,
  ;; 'daily, 'weekly, and 'monthly.
  ;; e.g. '((frequent . 8) (hourly . 12))
  (auto-snapshot-keep         zfs-configuration-auto-snapshot-keep
                              (default '())))

(define %default-auto-snapshot-keep
  '((frequent .  4)
    (hourly .    24)
    (daily .     31)
    (weekly .    8)
    (monthly .   12)))

(define %auto-snapshot-mcron-schedule
  '((frequent .  "0,15,30,45 * * * *")
    (hourly .    "0 * * * *")
    (daily .     "0 0 * * *")
    (weekly .    "0 0 * * 7")
    (monthly .   "0 0 1 * *")))

;; A synthetic and unusable MAPPED-DEVICE intended for use when
;; the user has created a mountable filesystem inside a ZFS
;; zvol and wants it mounted inside the configuration.scm.
(define %zfs-zvol-dependency
  (mapped-device
    (source '())
    (targets '("zvol/*"))
    (type #f)))

(define (make-zfs-package conf)
  "Creates a zfs package based on the given zfs-configuration.

  OpenZFS is a kernel package and to ensure best compatibility
  it should be compiled with the specific Linux-Libre kernel
  used on the system.  This simply overrides the kernel used
  in compilation with that given in the configuration, which
  the user has to ensure is the same as in the operating-system."
  (let ((kernel    (zfs-configuration-kernel conf))
        (base-zfs  (zfs-configuration-base-zfs conf)))
    (package
      (inherit base-zfs)
      (arguments (cons* #:linux kernel
                        (package-arguments base-zfs))))))

(define (make-zfs-auto-snapshot-package conf)
  "Creates a zfs-auto-snapshot package based on the given
  zfs-configuration.

  Since the OpenZFS tools above are compiled to a specific
  kernel version, zfs-auto-snapshot --- which calls into the
  OpenZFS tools --- has to be compiled with the specific
  modified OpenZFS package created in the make-zfs-package
  procedure."
  (let ((zfs                    (make-zfs-package conf))
        (base-zfs-auto-snapshot (zfs-configuration-base-zfs-auto-snapshot conf)))
    (package
      (inherit base-zfs-auto-snapshot)
      (inputs `(("zfs" ,zfs))))))

(define (zfs-loadable-modules conf)
  "Specifies that the specific 'module' output of the OpenZFS
  package is to be used; for use in indicating it as a
  loadable kernel module."
  (list (list (make-zfs-package conf) "module")))

(define (zfs-shepherd-services conf)
  "Constructs a list of Shepherd services that is installed
  by the ZFS Guix service.

  'zfs-scan' scans all devices for ZFS pools, and makes them
  available to 'zpool' commands.
  'device-mapping-zvol/*' waits for /dev/zvol/* to be
  populated by 'udev', and runs after 'zfs-scan'.
  'zfs-auto-mount' mounts all ZFS datasets with a 'mount'
  property, which defaults to '/' followed by the name of
  the dataset.

  All the above behavior is expected by ZFS users from
  typical ZFS installations.  A mild difference is that
  scanning is usually based on '/etc/zfs/zpool.cache'
  instead of the 'scan all devices' used below, but that
  file is questionable in Guix since ideally '/etc/'
  files are modified by the sysad directly;
  '/etc/zfs/zpool.cache' is modified by ZFS tools."
  (let* ((zfs-package     (make-zfs-package conf))
         (zpool           (file-append zfs-package "/sbin/zpool"))
         (zfs             (file-append zfs-package "/sbin/zfs"))
         (zvol_wait       (file-append zfs-package "/bin/zvol_wait"))
         (scheme-modules  `((srfi srfi-1)
                            (srfi srfi-34)
                            (srfi srfi-35)
                            (rnrs io ports)
                            ,@%default-modules)))
    (define zfs-scan
      (shepherd-service
        (provision '(zfs-scan))
        (requirement `(root-file-system
                       kernel-module-loader
                       udev
                       ,@(map dependency->shepherd-service-name
                              (zfs-configuration-dependencies conf))))
        (documentation "Scans for and imports ZFS pools.")
        (modules scheme-modules)
        (start #~(lambda _
                   (guard (c ((message-condition? c)
                              (format (current-error-port)
                                      "zfs: error importing pools: ~s~%"
                                      (condition-message c))
                              #f))
                     ;; TODO: optionally use a cachefile.
                     (invoke #$zpool "import" "-a" "-N"))))
        ;; Why not one-shot?  Because we don't really want to rescan
        ;; this each time a requiring process is restarted, as scanning
        ;; can take a long time and a lot of I/O.
        (stop #~(const #f))))

    (define device-mapping-zvol/*
      (shepherd-service
        (provision '(device-mapping-zvol/*))
        (requirement '(zfs-scan))
        (documentation "Waits for all ZFS ZVOLs to be opened.")
        (modules scheme-modules)
        (start #~(lambda _
                   (guard (c ((message-condition? c)
                              (format (current-error-port)
                                      "zfs: error opening zvols: ~s~%"
                                      (condition-message c))
                              #f))
                     (invoke #$zvol_wait))))
        (stop #~(const #f))))

    (define zfs-auto-mount
      (shepherd-service
        (provision '(zfs-auto-mount))
        (requirement '(zfs-scan))
        (documentation "Mounts all non-legacy mounted ZFS filesystems.")
        (modules scheme-modules)
        (start #~(lambda _
                   (guard (c ((message-condition? c)
                              (format (current-error-port)
                                      "zfs: error mounting file systems: ~s~%"
                                      (condition-message c))
                              #f))
                     ;; Output to current-error-port, otherwise the
                     ;; user will not see any prompts for passwords
                     ;; of encrypted datasets.
                     ;; XXX Maybe better to explicitly open /dev/console ?
                     (with-output-to-port (current-error-port)
                       (lambda ()
                         (invoke #$zfs "mount" "-a" "-l"))))))
        (stop #~(lambda _
                  ;; Make sure that Shepherd does not have a CWD that
                  ;; is a mounted ZFS filesystem, which would prevent
                  ;; unmounting.
                  (chdir "/")
                  (invoke #$zfs "unmount" "-a" "-f")))))

    `(,zfs-scan
      ,device-mapping-zvol/*
      ,@(if (zfs-configuration-auto-mount? conf)
            `(,zfs-auto-mount)
            '()))))

(define (zfs-user-processes conf)
  "Provides the last Shepherd service that 'user-processes' has to
  wait for.

  If not auto-mounting, then user-processes should only wait for
  the device scan."
  (if (zfs-configuration-auto-mount? conf)
      '(zfs-auto-mount)
      '(zfs-scan)))

(define (zfs-mcron-auto-snapshot-jobs conf)
  "Creates a list of mcron jobs for auto-snapshotting, one for each
  of the standard durations."
  (let* ((user-auto-snapshot-keep      (zfs-configuration-auto-snapshot-keep conf))
         ;; assoc-ref has earlier entries overriding later ones.
         (auto-snapshot-keep           (append user-auto-snapshot-keep
                                               %default-auto-snapshot-keep))
         (auto-snapshot?               (zfs-configuration-auto-snapshot? conf))
         (zfs-auto-snapshot-package    (make-zfs-auto-snapshot-package conf))
         (zfs-auto-snapshot            (file-append zfs-auto-snapshot-package
                                                    "/sbin/zfs-auto-snapshot")))
    (map
      (lambda (label)
        (let ((keep   (assoc-ref auto-snapshot-keep label))
              (sched  (assoc-ref %auto-snapshot-mcron-schedule label)))
          #~(job '#$sched
                 (lambda ()
                   (system* #$zfs-auto-snapshot
                            "--quiet"
                            "--syslog"
                            #$(string-append "--label="
                                             (symbol->string label))
                            #$(string-append "--keep="
                                             (number->string keep))
                            "//")))))
      (map first %auto-snapshot-mcron-schedule))))

(define (zfs-mcron-auto-scrub-jobs conf)
  "Creates a list of mcron jobs for auto-scrubbing."
  (let* ((zfs-package    (make-zfs-package conf))
         (zpool          (file-append zfs-package "/sbin/zpool"))
         (auto-scrub     (zfs-configuration-auto-scrub conf))
         (sched          (cond
                           ((eq? auto-scrub 'weekly)  "0 0 * * 7")
                           ((eq? auto-scrub 'monthly) "0 0 1 * *")
                           (else                      auto-scrub))))
    (define code
      ;; We need to get access to (guix build utils) for the
      ;; invoke procedures.
      (with-imported-modules (source-module-closure '((guix build utils)))
        #~(begin
            (use-modules (guix build utils)
                         (ice-9 ports))
            ;; The ZFS pools in the system.
            (define pools
              (invoke/quiet #$zpool "list" "-o" "name" "-H"))
            ;; Only scrub if there are actual ZFS pools, as the
            ;; zpool scrub command errors out if given an empty
            ;; argument list.
            (unless (null? pools)
              ;; zpool scrub only initiates the scrub and otherwise
              ;; prints nothing.  Results are always seen on the
              ;; zpool status command.
              (apply invoke #$zpool "scrub" pools)))))
    (list
      #~(job '#$sched
             #$(program-file "mcron-zfs-scrub.scm" code)))))

(define (zfs-mcron-jobs conf)
  "Creates a list of mcron jobs for ZFS management."
  (append (zfs-mcron-auto-snapshot-jobs conf)
          (if (zfs-configuration-auto-scrub conf)
              (zfs-mcron-auto-scrub-jobs conf)
              '())))

(define zfs-service-type
  (service-type
    (name 'zfs)
    (extensions
      (list ;; Install OpenZFS kernel module into kernel profile.
            (service-extension linux-loadable-module-service-type
                               zfs-loadable-modules)
            ;; And load it.
            (service-extension kernel-module-loader-service-type
                               (const '("zfs")))
            ;; Make sure ZFS pools and datasets are mounted at
            ;; boot.
            (service-extension shepherd-root-service-type
                               zfs-shepherd-services)
            ;; Make sure user-processes don't start until
            ;; after ZFS does.
            (service-extension user-processes-service-type
                               zfs-user-processes)
            ;; Install automated scrubbing and snapshotting.
            (service-extension mcron-service-type
                               zfs-mcron-jobs)

            ;; Install ZFS management commands in the system
            ;; profile.
            (service-extension profile-service-type
                               (compose list make-zfs-package))
            ;; Install ZFS udev rules.
            (service-extension udev-service-type
                               (compose list make-zfs-package))))
    (description "Installs ZFS, an advanced filesystem and volume manager.")))
