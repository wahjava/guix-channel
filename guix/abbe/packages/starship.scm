(define-module (abbe packages starship)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages pkg-config)
  #:use-module (guix gexp)
  #:use-module (abbe build-system nix-rust)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) :prefix license:))

(define-public starship
  (package
   (name "starship")
   (version "1.20.1")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/starship/starship")
                  (commit (string-append "v" version))))
            (sha256 (base32 "1mylqgpdbnh72a44plabf6vd6nj3pn7d5ijiqwgc2j7bfp8xgf33"))))
   (build-system nix-rust-build-system)
   (native-inputs (list cmake pkg-config))
   (inputs (list zlib))
   (arguments
    `(#:vendor-hash "0i34r22wsikg6nzzjh3drnssl7vwkbw2bd6y0nz03yifwbcj5n89"))
   (synopsis "The minimal, blazing-fast, and infinitely customizable prompt for any shell!")
   (description
    "The minimal, blazing-fast, and infinitely customizable prompt for any shell!")
   (home-page "https://starship.rs/")
   (license license:isc)))
