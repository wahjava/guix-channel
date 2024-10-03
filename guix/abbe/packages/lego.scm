(define-module (abbe packages lego)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (abbe build-system nix-go)
  #:use-module (abbe packages go)
  #:use-module ((guix licenses) #:prefix license:))

(define-public lego
  (package
    (name "lego")
    (version "4.19.0")
    (source (origin (method git-fetch)
                    (uri (git-reference
                          (url "https://github.com/go-acme/lego")
                          (commit (string-append "v" version))))
                    (sha256 (base32 "0bdaldmmbkpsy5sxgj10klv99wmg7bf98dc8x7kzcvxgrpp51bin"))))
    (build-system nix-go-build-system)
    (arguments
     `(#:vendor-hash "006vkz1naxsvkbzqk141fqk30js90f6a6jyvpryrlx2hkkr3zh85"
       #:sub-packages ("./cmd/lego")
       #:go ,go-123
       #:build-flags ("-trimpath")
       #:ldflags (list "-s" "-w" "-X" ,(string-append "main.version=" version))))
    (home-page "https://go-acme.github.io/lego/")
    (synopsis "Let's Encrypt/ACME client and library written in Go")
    (description "Let's Encrypt client and ACME library written in Go.")
    (license license:expat)))
