(define-module (abbe packages lego)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build-system copy)
  #:use-module (ice-9 match)
  #:use-module ((guix licenses) #:prefix license:))

(define (lego-url version)
  (match (%current-system)
    ("aarch64-linux"
     (string-append "https://github.com/go-acme/lego/releases/download/v" version
                    "/lego_v" version "_linux_arm64.tar.gz"))
    ("x86_64-linux"
     (string-append "https://github.com/go-acme/lego/releases/download/v" version
                    "/lego_v" version "_linux_amd64.tar.gz"))))

(define (lego-hash)
  (match (%current-system)
    ("x86_64-linux" "0izvcxxhm93c62diggikaph3v1gdnvvxql9ikrcjrydnyngxaqpk")
    ("aarch64-linux" "163f7xnh7kyw7hi52spy1rfw22by6xhj6dx34jakbhhmmlmkm75p")))

(define-public lego
  (package
    (name "lego")
    (version "4.17.4")
    (source (origin (method url-fetch/tarbomb)
                    (uri (lego-url version))
                    (sha256 (base32 (lego-hash)))))
    (build-system copy-build-system)
    (arguments
     `(#:install-plan
       '(("lego" "bin/lego"))))
    (home-page "https://go-acme.github.io/lego/")
    (synopsis "Let's Encrypt/ACME client and library written in Go")
    (description "Let's Encrypt client and ACME library written in Go.")
    (license license:expat)))
