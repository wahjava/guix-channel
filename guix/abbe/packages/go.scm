(define-module (abbe packages go)
  #:use-module (guix packages)
  #:use-module (nonguix build-system binary)
  #:use-module (guix download)
  #:use-module (ice-9 match)
  #:use-module (guix base16)
  #:use-module (guix licenses))

(define (go-hash version)
  (match (%current-system)
    ("aarch64-linux"
     (match version
       ("1.22.8" "5c616b32dab04bb8c4c8700478381daea0174dc70083e4026321163879278a4a")
       ("1.23.2" "f626cdd92fc21a88b31c1251f419c17782933a42903db87a174ce74eeecc66a9")))
    ("x86_64-linux"
     (match version
       ("1.22.8" "5f467d29fc67c7ae6468cb6ad5b047a274bae8180cac5e0b7ddbfeba3e47e18f")
       ("1.23.2" "542d3c1705f1c6a1c5a80d5dc62e2e45171af291e755d591c5e6531ef63b454e")))))

(define (go-file-suffix)
  (match (%current-system)
    ("aarch64-linux" "linux-arm64")
    ("x86_64-linux" "linux-amd64")))

(define (golang version)
  (package
    (name "golang")
    (version version)
    (source (origin
              (method url-fetch)
              (uri (string-append "https://go.dev/dl/go" version "." (go-file-suffix) ".tar.gz"))
              (sha256 (base16-string->bytevector (go-hash version)))))
    (build-system binary-build-system)
    (synopsis "Go language compiler")
    (description "Go language compiler")
    (home-page "https://go.dev")
    (license bsd-3)))

(define-public go-122 (golang "1.22.8"))

(define-public go-123 (golang "1.23.2"))
