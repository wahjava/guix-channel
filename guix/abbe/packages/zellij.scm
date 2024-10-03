(define-module (abbe packages zellij)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (abbe build-system nix-rust)
  #:use-module (guix git-download)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages pkg-config)
  #:use-module ((guix licenses) #:prefix license:))

(define %empty-hash (make-string 52 #\0))

(define-public zellij
  (package
    (name "zellij")
    (version "0.40.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/zellij-org/zellij")
                    (commit (string-append "v" version))))
              (sha256 (base32 "0nbbwwi2v8lpy20y5886rh8zq6l4vjyjqbwvzvq38nwa4yq31iwz"))))
    (build-system nix-rust-build-system)
    (native-inputs (list pkg-config))
    (inputs (list openssl))
    (arguments
     `(#:vendor-hash "0588xq2fyd2ax3r3d0awv6lf0508jq3r6nivimb1c8gsp4jf9g68"
       #:env-vars
       (list (cons "OPENSSL_NO_VENDOR" "yes")
             (cons "ZELLIJ_EXAMPLE_DIR" (string-append ,(package-source this-package) "/example"))
             (cons "ZELLIJ_ASSETS_DIR"  (string-append ,(package-source this-package) "/assets")))))

    (synopsis "A terminal workspace with batteries included")
    (description "Zellij is a workspace aimed at developers, ops-oriented people and anyone who loves the terminal. Similar programs are sometimes called \"Terminal Multiplexers\".")
    (home-page "https://zellij.dev")
    (license license:expat)))
