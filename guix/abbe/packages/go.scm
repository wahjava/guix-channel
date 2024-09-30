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
       ("1.22.7" "ed695684438facbd7e0f286c30b7bc2411cfc605516d8127dc25c62fe5b03885")
       ("1.23.1" "faec7f7f8ae53fda0f3d408f52182d942cc89ef5b7d3d9f23ff117437d4b2d2f")))
    ("x86_64-linux"
     (match version
       ("1.22.7" "fc5d49b7a5035f1f1b265c17aa86e9819e6dc9af8260ad61430ee7fbe27881bb")
       ("1.23.1" "49bbb517cfa9eee677e1e7897f7cf9cfdbcf49e05f61984a2789136de359f9bd")))))

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

(define-public go-122 (golang "1.22.7"))

(define-public go-123 (golang "1.23.1"))
