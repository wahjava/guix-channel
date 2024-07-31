(define-module (abbe packages linux)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages file-systems)
  #:use-module (gnu packages cpio))
;  #:use-module (nongnu packages linux))

;;;
;;; Linux-XanMod
;;;

(define* (make-linux-xanmod-source version xanmod-revision
                                   #:key xanmod-branch kernel-hash xanmod-hash)

  (define %upstream-linux-source
    (@@ (gnu packages linux) %upstream-linux-source))

  (define kernel-source
    (%upstream-linux-source (version-major+minor version) kernel-hash))

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
         (let* ((xz-name (basename #+xanmod-patch))
                (patch-xz-name (string-append (string-drop-right xz-name 3)
                                              ".patch.xz"))
                (patch-name (string-drop-right patch-xz-name 3)))
           (copy-file #+xanmod-patch patch-xz-name)
           (invoke #+(file-append xz "/bin/unxz") patch-xz-name)
           (invoke #+(file-append patch "/bin/patch")
                   "--force" "--no-backup-if-mismatch"
                   #+@(origin-patch-flags kernel-source)
                   "--input" patch-name)
           (for-each delete-file
                     (list patch-name
                           ;; EXTRAVERSION is used instead.
                           "localversion")))))))

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
                                  '(("CONFIG_BLK_DEV_NVME" . #t))))))
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
(define-public linux-xanmod-ng-version "6.10.2")
(define-public linux-xanmod-ng-revision "xanmod1")
(define-public linux-xanmod-ng-source
  (make-linux-xanmod-source
   linux-xanmod-ng-version
   linux-xanmod-ng-revision
   #:xanmod-branch "edge"
   #:kernel-hash (base32 "09p2z3z8c3aq6ipqdc58x6s52sy0cmyg6mj4f0g5yk755r19hikp")
   #:xanmod-hash (base32 "1blnhm9sy7dhdzxj0a9ak1l5ihin2frnx9ww1p0xd32jnx5fvqnf")))

;; Linux-XanMod packages
(define-public linux-xanmod-ng
  (make-linux-xanmod linux-xanmod-ng-version
                     linux-xanmod-ng-revision
                     linux-xanmod-ng-source
                     #:name "linux-xanmod-ng"
                     #:defconfig "config_x86-64-v3"))

(define-public lkm-tuxedo-keyboard-xanmod-ng
  (list
   (package/inherit
    tuxedo-keyboard
    (name "tuxedo-keyboard-linux-xanmod-ng")
    (arguments
     (substitute-keyword-arguments (package-arguments tuxedo-keyboard)
       ((#:linux original-linux linux-xanmod-ng)
        linux-xanmod-ng))))))

(define-public ddcci-xanmod-ng
  (package/inherit
   ddcci-driver-linux
   (arguments
    (substitute-keyword-arguments (package-arguments ddcci-driver-linux)
      ((#:linux original-linux linux-xanmod-ng)
       linux-xanmod-ng)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public linux-xanmod-version "6.9.12")
(define-public linux-xanmod-revision "xanmod1")
(define-public linux-xanmod-source
  (make-linux-xanmod-source
   linux-xanmod-version
   linux-xanmod-revision
   #:defconfig "config_x86-64-v4"
   #:xanmod-branch "main"
   #:kernel-hash (base32 "0jc14s7z2581qgd82lww25p7c4w72scpf49z8ll3wylwk3xh3yi4")
   #:xanmod-hash (base32 "1n68ylzvcv7sjdlcpfixw79fbinsaywh9svg0v6npdv1afkz95j0")))


(define-public linux-xanmod
  (make-linux-xanmod linux-xanmod-version
                     linux-xanmod-revision
                     linux-xanmod-source))

(define-public zfs-xanmod
  (list
   (package/inherit
    zfs
    (arguments
     (substitute-keyword-arguments (package-arguments zfs)
       ((#:linux original-linux linux-xanmod)
        linux-xanmod))))))

(define-public ddcci-xanmod
  (package/inherit
   ddcci-driver-linux
   (arguments
    (substitute-keyword-arguments (package-arguments ddcci-driver-linux)
      ((#:linux original-linux linux-xanmod)
       linux-xanmod)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
