(define-module (abbe packages flyctl)
  #:use-module (guix packages)
  #:use-module (gnu packages linux)
  #:use-module (guix gexp)
  #:use-module (guix build-system copy)
  #:use-module (guix download)
  #:use-module (guix licenses)
  #:use-module (guix base16)
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
    ("aarch64-linux" "049d666d26f916ae636b752defb3a705e7ba055ecef014399ea5ac766a1bb1e1")
    ("x86_64-linux" "3afb88f7c65aed434c40befac984eacc081e2b328500255c5095a5f6abd86f7b")))

(define-public flyctl
  (package
   (name "flyctl")
   (version "0.2.99")
   (source (origin
            (method url-fetch/tarbomb)
            (uri (flyctl-url version (%current-system)))
            (sha256 (base16-string->bytevector (flyctl-hash (%current-system))))))
   (build-system copy-build-system)
   (arguments
    `(#:install-plan
      ,#~(list '("flyctl" "bin/"))))
   (synopsis "Command line tools for fly.io services")
   (description
    "flyctl is a command-line interface for fly.io")
   (home-page "https://fly.io")
   (license asl2.0)))
