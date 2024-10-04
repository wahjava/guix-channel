(define-module (abbe packages wush)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (abbe packages go)
  #:use-module (abbe build-system nix-go)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) :prefix license:)
  #:use-module (ice-9 match))

(define-public wush
  (package
    (name "wush")
    (version "0.3.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/coder/wush")
                    (commit (string-append "v" version))))
              (sha256 (base32 "13lfz0j95qszydaxzz7hmaabs9dn06xf8w5ayfr1q7brkpb5wqfs"))))
    (build-system nix-go-build-system)
    (license license:cc0)
    (arguments
     `(#:vendor-hash "1mkj7v9aq69jdxvhr7ay059x9ldgv4jqr31i8jf48ypxlc64739y"
       #:build-flags ("-trimpath")
       #:sub-packages ("./cmd/wush")
       #:ldflags
       '("-s" "-w" "-X"
         ,(string-append "main.version=" version)
         "-X" "main.commit=f7950c479f3c63843dcf4f2f60ba0a6c9149cc37"
         "-X" "main.commitDate=1970-01-01T00:00:00Z")))
    (home-page "https://wush.dev")
    (description "wush is a command line tool that lets you easily transfer files and open shells over a peer-to-peer WireGuard connection. It's similar to magic-wormhole.")
    (synopsis "simplest & fastest way to transfer files between computers via WireGuard")))
