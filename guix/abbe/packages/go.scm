(define-module (abbe packages go)
  #:use-module (guix packages)
  #:use-module (nonguix build-system binary)
  #:use-module (guix download)
  #:use-module (ice-9 match)
  #:use-module (guix licenses))

(define (go-hash)
  (match (%current-system)
    ("aarch64-linux" "0nibchqlxr16r10m1pxbbx9p3bfrscwilz2jc0vbwcglzidk48cd")
    ("x86_64-linux" "1w67wkk0jl780z2ricf9v0dl9sljn4sk5ijva63a1bjy8d6r4jwh")))

(define (go-file-suffix)
  (match (%current-system)
    ("aarch64-linux" "linux-arm64")
    ("x86_64-linux" "linux-amd64")))

(define-public go-122
  (package
   (name "golang")
   (version "1.22.5")
   (source (origin
            (method url-fetch)
            (uri (string-append "https://go.dev/dl/go" version "." (go-file-suffix) ".tar.gz"))
            (sha256 (base32 (go-hash)))))
   (build-system binary-build-system)
   (synopsis "Go language compiler")
   (description "Go language compiler")
   (home-page "https://go.dev")
   (license bsd-3)))
