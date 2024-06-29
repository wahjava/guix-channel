(define-module (abbe packages aerc)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages python)
  #:use-module (nonguix build-system binary)
  #:use-module (guix download)
  #:use-module (guix licenses))

(define-public aerc-bin
  (package
    (name "aerc")
    (version "0.17.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
		    "https://www.lostca.se/~abbe/" name "-" version
		    ".tar.xz"))
	      (sha256 (base32 "0ac5acchbf1ij6i4nr4vzxz9y1pv72x73igdnjd02r749g9wjwcp"))))
    (build-system binary-build-system)
    (arguments
      (list #:patchelf-plan
	    `'(("bin/aerc" ("gcc"))
	      ("libexec/aerc/filters/colorize" ("gcc"))
	      ("libexec/aerc/filters/wrap" ("gcc")))))
    (inputs `((,gcc-11 "lib") ,python))
    (synopsis "aerc is an email client for your terminal.")
    (description "aerc is an email client for your terminal.")
    (home-page "https://aerc-mail.org/")
    (license expat)))
