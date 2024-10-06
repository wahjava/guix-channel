(define-module (abbe packages linux)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages file-systems)
  #:use-module (gnu packages cpio)
  #:use-module (nongnu packages firmware)
  #:use-module (abbe packages curl))
;  #:use-module (nongnu packages linux))

(define-public fwupd-nonfree-with-curl-http3
  (package/inherit fwupd-nonfree
                   (propagated-inputs
                    (modify-inputs (package-propagated-inputs fwupd-nonfree)
                                   (replace "curl" curl-http3)))))

;;;
;;; Linux-XanMod
;;;

(define* (make-linux-xanmod-source version xanmod-revision
                                   #:key xanmod-branch kernel-hash xanmod-hash bore)

  (define %upstream-linux-source
    (@@ (gnu packages linux) %upstream-linux-source))

  (define kernel-source
    (%upstream-linux-source (version-major+minor version) kernel-hash))

  (define bore-patches
    (let ((linux-xanmod-bore-base-url "https://raw.githubusercontent.com/micros24/linux-xanmod-bore/refs/heads/6.11/"))
      (if (not bore) (list)
        (list (origin
                (method url-fetch)
                (uri (string-append linux-xanmod-bore-base-url "0001-bore.patch"))
                (file-name "linux-xanmod-bore-0001-bore.patch")
                (sha256 (base32 "15fm2hk0ikjdvyinrm5j1yf4cvdq1fhs4nhxkzvv6303nlr83m91")))

              (origin
                (method url-fetch)
                (uri (string-append linux-xanmod-bore-base-url "0002-glitched-cfs.patch"))
                (file-name "linux-xanmod-bore-0002-glitched-cfs.patch")
                (sha256 (base32 "0wl86q7bnjar771zp34v6f7qzw2hpbv7aiya6p66y5a23375hchz")))

              (origin
                (method url-fetch)
                (uri (string-append linux-xanmod-bore-base-url "0003-glitched-eevdf-additions.patch"))
                (file-name "linux-xanmod-bore-0003-glitched-eevdf-additions.patch")
                (sha256 (base32 "0yq5sghbrv58zchsq315a3680ccr3i8mw2m0wc4ghsr63pd3yp4g")))))))

  (define xanmod-patch
    (origin
      (method url-fetch)
      (uri (string-append
            "mirror://sourceforge/xanmod/releases/" xanmod-branch "/"
            version "-" xanmod-revision "/patch-"
            version "-" xanmod-revision ".xz"))
      (sha256 xanmod-hash)))

  (origin
    (inherit kernel-source)
    (modules '((guix build utils)))
    (snippet
     #~(begin
         (let ((apply-patch
                (lambda (orig-patch-name)
                  (let* ((patch-base-name (basename orig-patch-name))
                         (patch-name
                          (if (string-suffix? ".xz" patch-base-name)
                              (let* ((patch-xz-name (string-append (string-drop-right patch-base-name 3)
                                                                   ".patch.xz"))
                                     (uncompressed-patch-name (string-drop-right patch-xz-name 3)))
                                (copy-file orig-patch-name patch-xz-name)
                                (invoke #+(file-append xz "/bin/unxz") patch-xz-name)
                                uncompressed-patch-name)

                              (begin
                                (copy-file orig-patch-name patch-base-name)
                                patch-base-name))))

                    (invoke #+(file-append patch "/bin/patch")
                            "--force" "--no-backup-if-mismatch"
                            #+@(origin-patch-flags kernel-source)
                            "--input" patch-name)

                    (delete-file patch-name)))))

           (for-each apply-patch
                     (list #+@(cons xanmod-patch bore-patches)))

           (delete-file "localversion"))))))

(define* (make-linux-xanmod version xanmod-revision source
                            #:key
                            (name "linux-xanmod")
                            (xanmod-defconfig "config_x86-64-v1"))

  (define %default-extra-linux-options
    ((@@ (gnu packages linux) default-extra-linux-options) version))

  (define config->string
    (@@ (gnu packages linux) config->string))

  (define base-kernel
    (customize-linux
     #:name name
     #:source source
     #:defconfig xanmod-defconfig
     ;; EXTRAVERSION is used instead.
     #:configs (config->string
                '(("CONFIG_LOCALVERSION" . "")))
     #:extra-version xanmod-revision))

  (package
    (inherit base-kernel)
    (version version)
    (arguments
     (substitute-keyword-arguments (package-arguments base-kernel)
       ((#:phases phases)
        #~(modify-phases #$phases
            ;; Since `customize-linux' replaces the configure phase, we add
            ;; XanMod defconfig beforehand to ensure compatibility of the
            ;; resulting package with `customize-linux'.
            (add-before 'configure 'add-xanmod-defconfig
              (lambda _
                (rename-file
                 (string-append "CONFIGS/xanmod/gcc/" #$xanmod-defconfig)
                 ".config")

                ;; Adapted from `make-linux-libre*'.
                (chmod ".config" #o666)
                (let ((port (open-file ".config" "a"))
                      (extra-configuration
                       #$(config->string
                          (append %default-extra-linux-options
                                  ;; NOTE: These are configs expected by Guix
                                  ;; but missing from XanMod defconfig.
                                  '(("CONFIG_BLK_DEV_NVME" . #t)
                                    ("CONFIG_ENABLE_DEFAULT_TRACERS" . #t)
                                    ("CONFIG_FTRACE_SYSCALLS" . #t)
                                    ("CONFIG_STACK_TRACER" . #t)
                                    ("CONFIG_DYNAMIC_FTRACE" . #t)
                                    ("CONFIG_FUNCTION_PROFILER" . #t)
                                    ("CONFIG_FUNCTION_GRAPH_TRACER" . #t)
                                    ("CONFIG_FUNCTION_TRACER" . #t)
                                    ("CONFIG_SCHED_BORE" . #t)
                                    ("CONFIG_FTRACE" . #t))))))
                  (display extra-configuration port)
                  (close-port port))
                (invoke "make" "oldconfig")

                (rename-file
                 ".config"
                 (string-append "arch/x86/configs/" #$xanmod-defconfig))))))))
    (native-inputs
     (modify-inputs (package-native-inputs base-kernel)
       ;; cpio is needed for CONFIG_IKHEADERS.
       (prepend cpio zstd)))
    (home-page "https://xanmod.org/")
    (supported-systems '("x86_64-linux"))
    (synopsis "Linux kernel distribution with custom settings and new features")
    (description
     "This package provides XanMod kernel, a general-purpose Linux kernel
distribution with custom settings and new features.  It's built to provide a
stable, responsive and smooth desktop experience.")))

;; Linux-XanMod sources
(define-public linux-xanmod-ng-version "6.11.2")
(define-public linux-xanmod-ng-revision "xanmod1")
(define-public linux-xanmod-ng-source
  (make-linux-xanmod-source
   linux-xanmod-ng-version
   linux-xanmod-ng-revision
   #:xanmod-branch "main"
   #:bore #t
   #:kernel-hash (base32 "0bnbvadm4wvnwzcq319gsgl03ijvvljn7mj8qw87ihpb4p0cdljm")
   #:xanmod-hash (base32 "1v36vqz8q3p493gk0p3pz3rnw5agzrnxznhih8wz6wxgw5jywna8")))

;; Linux-XanMod packages
(define-public linux-xanmod-ng-v3
  (make-linux-xanmod linux-xanmod-ng-version
                     linux-xanmod-ng-revision
                     linux-xanmod-ng-source
                     #:name "linux-xanmod-ng"
                     #:xanmod-defconfig "config_x86-64-v3"))

(define-public linux-xanmod-ng-v4
  (make-linux-xanmod linux-xanmod-ng-version
                     linux-xanmod-ng-revision
                     linux-xanmod-ng-source
                     #:name "linux-xanmod-ng"
                     #:xanmod-defconfig "config_x86-64-v4"))

(define-public lkm-tuxedo-keyboard-xanmod-ng
   (package/inherit
    tuxedo-keyboard
    (arguments
     (substitute-keyword-arguments (package-arguments tuxedo-keyboard)
       ((#:linux original-linux linux-xanmod-ng-v3)
        linux-xanmod-ng-v3)))))

(define ddcci-driver-linux-patched
  (let ((fix-611-patch
          (origin (method url-fetch)
                  (uri "https://gitlab.com/ddcci-driver-linux/ddcci-driver-linux/-/merge_requests/17.patch")
                  (sha256 (base32 "1basj6nqpmdbbbrlf2j48kfcgswy1ci7vmywg4wm785mrs618vjx"))
                  (file-name "ddcci-fix-on-6-11.patch")))
        (ddcci-driver-linux
          (package/inherit ddcci-driver-linux
            (source (origin
                     (method git-fetch)
                     (uri (git-reference
                            (url "https://gitlab.com/ddcci-driver-linux/ddcci-driver-linux")
                            (commit "0233e1ee5eddb4b8a706464f3097bad5620b65f4")))
                     (sha256 (base32 "1kszs2b6p0brzpc4nah0606hxssg7qka337bkhgff4qlvy7fijrs")))))))
    (package-with-extra-patches ddcci-driver-linux (list fix-611-patch))))

(define-public ddcci-xanmod-ng-v3
  (package/inherit
   ddcci-driver-linux-patched
   (arguments
    (substitute-keyword-arguments (package-arguments ddcci-driver-linux)
      ((#:linux original-linux linux-xanmod-ng-v3)
       linux-xanmod-ng-v3)))))

(define-public ddcci-xanmod-ng-v4
  (package/inherit
   ddcci-driver-linux-patched
   (arguments
    (substitute-keyword-arguments (package-arguments ddcci-driver-linux)
      ((#:linux original-linux linux-xanmod-ng-v4)
       linux-xanmod-ng-v4)))))

(define-public bpftool-xanmod-ng
               (package/inherit bpftool
                                (source  (package-source linux-xanmod-ng-v3))
                                (version (package-version linux-xanmod-ng-v3))))

(define-public bpftool-xanmod-ng-v4
               (package/inherit bpftool
                                (source  (package-source linux-xanmod-ng-v4))
                                (version (package-version linux-xanmod-ng-v4))))

(define-public zfs-xanmod-ng
  (package/inherit zfs
    (arguments
     (substitute-keyword-arguments (package-arguments zfs)
       ((#:linux original-linux linux-xanmod-ng-v4)
        linux-xanmod-ng-v4)))))

(define-public zfs-auto-snapshot-xanmod-ng
   (package/inherit
    zfs-auto-snapshot
    (inputs (modify-inputs (package-inputs zfs-auto-snapshot)
                           (replace "zfs" zfs-xanmod-ng)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
