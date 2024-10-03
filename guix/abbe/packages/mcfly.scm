(define-module (abbe packages mcfly)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (abbe build-system nix-rust)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:))

(define-public mcfly
  (package
    (name "mcfly")
    (version "0.9.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/cantino/mcfly")
                    (commit (string-append "v" version))))
              (sha256 (base32 "0ak43skci7cviimgqpjmrh26ng3bsk5ia57vjcl88nnk3k5jh219"))))
    (build-system nix-rust-build-system)
    (arguments
     `(#:vendor-hash "1xsi79wr4g1y1nkhhqhjjjczf5g2vvis657qgvc7kw1i10iqcn00"))
    (synopsis "Fly through your shell history")
    (description "Fly through your shell history")
    (home-page "https://github.com/cantino/mcfly")
    (license license:expat)))
