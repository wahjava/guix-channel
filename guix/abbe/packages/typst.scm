(define-module (abbe packages typst)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (nonguix build-system binary)
  #:use-module (guix download)
  #:use-module (guix licenses))

(define-public typst
  (package
   (name "typst")
   (version "0.11.1")
   (source (origin
            (method url-fetch)
            (uri (string-append
		  "https://github.com/typst/typst/releases/download/v"
		  version
		  "/typst-aarch64-unknown-linux-musl.tar.xz"))
	    (sha256 (base32 "09jbbia7i2i217wcgzfm43jgvpdcrn83ys1ys7zynvxj6zxzf53z"))))
   (build-system binary-build-system)
   (arguments
    `(#:install-plan
      '(("typst" "bin/typst"))))
   (synopsis "Typst is a typesetting app")
   (description "Typst is a typesetting app")
   (home-page "https://typst.app/")
   (license expat)))
