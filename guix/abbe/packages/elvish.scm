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
         ("x86_64-linux" "01ybinl2r70dzhnq8dmd1g3r4gzpglqh933fkwzq7wpqgavig5mx")
         ("aarch64-linux" "1rqks9z8rndkxhnfvbj5wgj6c9gr00gfkm116zahvdzhwak6kcbx")))

(define-public elvish
               (package
                 (name "elvish")
                 (version "0.20.1")
                 (source (origin
                           (method url-fetch/tarbomb)
                           (uri (elvish-url version))
                           (sha256 (base32 (elvish-hash version)))))
                 (build-system copy-build-system)
                 (arguments
                   (list #:install-plan
                     `'((,(string-append "elvish-v" version) "bin/elvish"))))
                 (home-page "https://elv.sh")
                 (synopsis "Interactive shell, and a scripting language.")
                 (description "A shell with useful interactive features built-in.")
                 (license license:bsd-2)))
