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
    ("aarch64-linux" "0j91409ggm8vpwpb407vivz3plkbrid8gmmha2i02ikglyxkpws2")
    ("x86_64-linux" "1s01jgwa2pmcx7315scjlkm9lwqzlawc2a2hgs6gz8azxg1pd04b")))

(define-public flyctl
  (package
   (name "flyctl")
   (version "0.2.72")
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
