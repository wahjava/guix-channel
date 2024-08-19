(define-module (abbe packages elvish)
               #:use-module (guix packages)
               #:use-module (guix download)
               #:use-module (guix gexp)
               #:use-module (ice-9 match)
               #:use-module (guix build-system copy)
               #:use-module ((guix licenses) #:prefix license:))

(define (elvish-url version)
  (match (%current-system)
         ("x86_64-linux" (string-append "https://dl.elv.sh/linux-amd64/elvish-v" version ".tar.gz"))
         ("aarch64-linux" (string-append "https://dl.elv.sh/linux-arm64/elvish-v" version ".tar.gz"))))

(define (elvish-hash version)
  (match (%current-system)
         ("x86_64-linux" "0wkmiyspfdy8ks5zn23hq84a6xya4fnp06hcvlxispfsvld9d3yp")
         ("aarch64-linux" "1vip0jkw6h3n6kj6vsfqc1db4lkrz3dz0wxs8i9mdrfgvgdwjs2k")))

(define-public elvish
               (package
                 (name "elvish")
                 (version "0.21.0")
                 (source (origin
                           (method url-fetch/tarbomb)
                           (uri (elvish-url version))
                           (sha256 (base32 (elvish-hash version)))))
                 (build-system copy-build-system)
                 (arguments
                   `(#:install-plan
                     '(("elvish" "bin/elvish"))))
                 (home-page "https://elv.sh")
                 (synopsis "Interactive shell, and a scripting language.")
                 (description "A shell with useful interactive features built-in.")
                 (license license:bsd-2)))
