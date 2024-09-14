(define-module (abbe packages carapace)
  #:use-module (guix packages)
  #:use-module (gnu packages linux)
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module ((guix licenses) :prefix license:)
  #:use-module (guix build-system copy)
  #:use-module (ice-9 match))

(define (carapace-arch system)
  (match system
    ("aarch64-linux" "arm64")
    ("x86_64-linux" "amd64")
    ("i686-linux" "386")))

(define (carapace-url version system)
  (let ([system (carapace-arch system)])
    (string-append "https://github.com/carapace-sh/carapace-bin/releases/download/v"
                   version "/carapace-bin_linux_" system ".tar.gz")))

(define (carapace-hash system)
  (match system
    ("aarch64-linux" "06aq72dhnjg2iys8fwirqp5gm2yqhwdmx34snwfrisrqwglmgc6m")
    ("x86_64-linux" "0yag4w46mz23rf5i7834a9fgkv03kw3vaqf41x9mswckywl54297")
    ("i686-linux" "1vql1gzwa63amg4qgvc6a14g1bdzfinsvp0sks8i8d8h24pc4y7c")))

(define-public carapace
  (package
   (name "carapace")
   (version "1.0.6")
   (source (origin
            (method url-fetch/tarbomb)
            (uri (carapace-url version (%current-system)))
            (sha256 (base32 (carapace-hash (%current-system))))))
   (build-system copy-build-system)
   (arguments
    (list
     #:install-plan
     #~`(("carapace" "bin/"))))
   (synopsis "multi-shell multi-command argument completer")
   (description
    "Carapace-bin provides argument completion for multiple CLI commands (full list), and works across multiple POSIX and non-POSIX shells.")
   (home-page "https://carapace.sh/")
   (license license:expat)))
