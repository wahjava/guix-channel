(define-module (abbe packages flyctl)
  #:use-module (guix packages)
  #:use-module (gnu packages linux)
  #:use-module (guix gexp)
  #:use-module (guix build-system copy)
  #:use-module (guix download)
  #:use-module (guix licenses)
  #:use-module (ice-9 match))

(define (flyctl-arch system)
  (match system
    ("aarch64-linux" "arm64")
    ("x86_64-linux" "x86_64")))

(define (flyctl-url version system)
  (let ([system (flyctl-arch system)])
    (string-append "https://github.com/superfly/flyctl/releases/download/v" version
		   "/flyctl_" version "_Linux_" system ".tar.gz")))

(define (flyctl-hash system)
  (match system
    ("aarch64-linux" "1qncnk6cv001xly9h5akvaivqz9aprqakc57r9v5x945xwh5li59")
    ("x86_64-linux" "0a14cdff3xmcyq111l9zla8hdsxrh22izixzd8gm4wk1xhf1wqa1")))

(define-public flyctl
  (package
   (name "flyctl")
   (version "0.2.65")
   (source (origin
            (method url-fetch/tarbomb)
            (uri (flyctl-url version (%current-system)))
            (sha256 (base32 (flyctl-hash (%current-system))))))
   (build-system copy-build-system)
   (arguments
    `(#:install-plan
      ,#~(list '("flyctl" "bin/"))))
   (synopsis "Command line tools for fly.io services")
   (description
    "flyctl is a command-line interface for fly.io")
   (home-page "https://fly.io")
   (license asl2.0)))
