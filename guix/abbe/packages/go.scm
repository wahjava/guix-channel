(define-module (abbe packages go)
  #:use-module (guix packages)
  #:use-module (nonguix build-system binary)
  #:use-module (guix download)
  #:use-module (ice-9 match)
  #:use-module (guix licenses))

(define (go-hash)
  (match (%current-system)
    ("aarch64-linux" "0wg7z71f189z1qbs7v27ha7kxsi75rba7b1022va3r6jak1pgqd8")
    ("x86_64-linux" "0zfypbkqbnsr66ghjaww0hg6b2j1raf265il4yb52mq2c59d8yds")))

(define (go-file-suffix)
  (match (%current-system)
    ("aarch64-linux" "linux-arm64")
    ("x86_64-linux" "linux-amd64")))

(define-public go-122
  (package
   (name "go")
   (version "1.22.4")
   (source (origin
            (method url-fetch)
            (uri (string-append "https://go.dev/dl/go" version "." (go-file-suffix) ".tar.gz"))
            (sha256 (base32 (go-hash)))))
   (build-system binary-build-system)
   (synopsis "Go language compiler")
   (description "Go language compiler")
   (home-page "https://go.dev")
   (license bsd-3)))
