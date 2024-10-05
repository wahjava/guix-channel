(define-module (abbe packages iamb)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (abbe build-system nix-rust)
  #:use-module (abbe packages openssl)
  #:use-module (gnu packages pkg-config)
  #:use-module ((guix licenses) #:prefix license:))

(define-public iamb
  (package
    (name "iamb")
    (version "0.0.10")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/ulyssa/iamb")
                    (commit (string-append "v" version))))
	            (sha256 (base32 "026gv6mgaa95qczigyaw8cb4wdyrssajgplrkr5hfzv081cm4c3j"))))
    (build-system nix-rust-build-system)
    (arguments
     `(#:vendor-hash "1k54jxmdwd96zyaymji3ci9bkkrcgyi74k19h0v6rwvy97r4n21f"
       #:env-vars
       (list '("OPENSSL_NO_VENDOR" . "yes")
             '("VERGEN_GIT_SHA" . "2e6376ff866a17eb4727af039b6a8e801da252e2"))))
    (inputs (list openssl-3.3))
    (native-inputs
     (list (list "pkg-config" pkg-config)))
    (synopsis "A terminal-based client for Matrix for the Vim addict.")
    (description "iamb is a terminal-based client for Matrix for the Vim addict. You can edit messages, navigate windows and manage tabs in the same ways that your fingers are used to from your favorite text editor!")
    (home-page "https://iamb.chat/")
    (license license:asl2.0)))
