(define-module (abbe packages mcfly)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (nonguix build-system binary)
  #:use-module (guix download)
  #:use-module (guix licenses))

(define helix-version "24.03")
(define helix-base-dir ".")

(define-public mcfly
  (package
   (name "mcfly")
   (version "0.8.6")
   (source (origin
            (method url-fetch/tarbomb)
            (uri (string-append
		  "https://github.com/cantino/mcfly/releases/download/v"
		  version
		  "/mcfly-v"
		  version
		  "-aarch64-unknown-linux-musl.tar.gz"))
	    (sha256 (base32 "0sml4zr9qg8b5d6c9xi3vqi2cdqf74cib7l5czy08d918yh2nd8g"))))
   (build-system binary-build-system)
   (arguments
    '(#:install-plan
      '(("mcfly" "bin/mcfly"))))

   (synopsis "Fly through your shell history")
   (description "Fly through your shell history")
   (home-page "https://github.com/cantino/mcfly")
   (license expat)))
