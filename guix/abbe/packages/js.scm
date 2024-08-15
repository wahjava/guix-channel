(define-module (abbe packages js)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (ice-9 match)
  #:use-module (guix base16)
  #:use-module (nonguix build-system binary)
  #:use-module ((gnu packages compression) #:select (unzip))
  #:use-module ((guix licenses) #:prefix license:))

(define bun-version "1.1.24")

(define (bun-url)
  (match (%current-system)
    ("aarch64-linux" (string-append "https://github.com/oven-sh/bun/releases/download/bun-v"
                                    bun-version
                                    "/bun-linux-aarch64.zip"))
    ("x86_64-linux" (string-append "https://github.com/oven-sh/bun/releases/download/bun-v"
                                    bun-version
                                    "/bun-linux-x64.zip"))))

(define (bun-hash)
  (match (%current-system)
    ("aarch64-linux" "82e73030cb3b6399ea84fcd8603a034783f357c6af7d7959e9a7bcafbc1f66a0")
    ("x86_64-linux" "f478880678cf282fdda46cc9d58e20a0dde1fad8c9cfee128ead43996453e1c8")))

(define-public bun
  (package
    (name "bun")
    (version bun-version)
    (source (origin (method url-fetch)
                    (uri (bun-url))
                    (sha256 (base16-string->bytevector (bun-hash)))))
    (native-inputs (list unzip))
    (build-system binary-build-system)
    (arguments
     `(#:install-plan
       '(("bun" "bin/"))
       #:patchelf-plan
       '(("bun"))))
    (home-page "https://bun.sh")
    (description "Bun is an all-in-one toolkit for JavaScript and TypeScript apps. It ships as a single executable called bun.")
    (synopsis "Incredibly fast JavaScript runtime, bundler, test runner, and package manager - all in one")
    (license license:expat)))
