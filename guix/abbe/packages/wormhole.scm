(define-module (abbe packages wormhole)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix build utils)
  #:use-module (abbe build-system nix-go)
  #:use-module (abbe build-system nix-rust)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) :prefix license:))

(define-public magic-wormhole-rs
  (package
    (name "magic-wormhole-rs")
    (version "0.7.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/magic-wormhole/magic-wormhole.rs")
                    (commit version)))
              (sha256 (base32 "1in565h4cklpm6d4pfqflipiqg1vg6axjqh8n343858n029nnjmv"))))
    (build-system nix-rust-build-system)
    (arguments
     '(#:vendor-hash "086wbrlz4gwvrfq8y1dgliymd28qf1wz7hj3jfk8aayhppxb6l51"
       #:cargo-install-paths ("./cli")
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-manpages
           (lambda* (#:key outputs :#allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (install-file "wormhole.1"
                             (string-append out "/share/man/man1"))))))))
    (home-page "https://github.com/magic-wormhole/magic-wormhole.rs")
    (synopsis "Rust implementation of Magic Wormhole, with new features and enhancements")
    (description "Get things from one computer to another, safely.")
    (license license:eupl1.2)))

(define-public wormhole-william
  (package
    (name "wormhole-william")
    (version "1.0.7")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/psanford/wormhole-william")
                    (commit (string-append "v" version))))
              (sha256 (base32 "0z2qnlhhszgqbw8y1v6v41869pxg1vxcbbjpw60fa86ww9jzvf18"))))
    (build-system nix-go-build-system)
    (license license:expat)
    (arguments
     '(#:vendor-hash "1jv86986y61zdmh5da8n1qddsfb81bra2sbpl4lgxf331cggp750"
       #:build-flags ("-trimpath")))
    (home-page "https://github.com/psanford/wormhole-william")
    (description "wormhole-william is a Go (golang) implementation of magic wormhole. It provides secure end-to-end encrypted file transfers between computers. The endpoints are connected using the same \"wormhole code\".")
    (synopsis "End-to-end encrypted file transfer. A magic wormhole CLI and API in Go")))
