(define-module (abbe packages eza)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (nonguix build-system binary)
  #:use-module (guix download)
  #:use-module (guix base16)
  #:use-module (ice-9 match)
  #:use-module (gnu packages gcc)
  #:use-module (guix licenses))

(define (eza-arch system)
  (match system
    ("aarch64-linux" "aarch64-unknown-linux-gnu")
    ("x86_64-linux" "x86_64-unknown-linux-gnu")))

(define (eza-url version system)
  (let ([system (eza-arch system)])
    (string-append
     "https://github.com/eza-community/eza/releases/download/v" version "/eza_" system ".tar.gz")))

(define (eza-hash system)
  (match system
    ("x86_64-linux" "b4351b3eaa7849aadb005b5769efac120c52a58851c3365fc8de6dcb6721c316")
    ("aarch64-linux" "14965a8cd5325d8e61e3ea4e12b40c4232d1436728a73242b361cdcdecef1032")))

(define-public eza
  (package
   (name "eza")
   (version "0.18.23")
   (source (origin
            (method url-fetch/tarbomb)
            (uri (eza-url version (%current-system)))
            (sha256 (base16-string->bytevector (eza-hash (%current-system))))))
   (build-system binary-build-system)
   (inputs (list (list gcc "lib")))
   (arguments
    '(#:install-plan
      '(("eza" "bin/eza"))
      #:patchelf-plan
      `(("eza" ("gcc")))))

   (synopsis "A modern alternative to ls.")
   (description "eza is a modern, maintained replacement for the venerable file-listing
command-line program \"ls\" that ships with Unix and Linux operating systems,
giving it more features and better defaults.")
   (home-page "https://eza.rocks/")
   (license expat)))
