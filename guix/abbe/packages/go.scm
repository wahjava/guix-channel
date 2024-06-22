(define-module (abbe packages go)
  #:use-module (guix packages)
  #:use-module (nonguix build-system binary)
  #:use-module (guix download)
  #:use-module (guix licenses))


(define-public go-122
  (package
   (name "go")
   (version "1.22.4")
   (source (origin
            (method url-fetch)
            (uri (string-append "https://go.dev/dl/go" version ".linux-amd64.tar.gz"))
            (sha256 (base32 "0zfypbkqbnsr66ghjaww0hg6b2j1raf265il4yb52mq2c59d8yds"))))
   (build-system binary-build-system)
   (synopsis "Go language compiler")
   (description "Go language compiler")
   (home-page "https://go.dev")
   (license bsd-3)))
