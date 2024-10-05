(define-module (abbe packages sagittarius-scheme)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix build utils)
  #:use-module (guix packages)
  #:use-module ((guix licenses) :prefix license:)
  #:use-module (abbe packages openssl)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages bdw-gc)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages pkg-config))

(define-public sagittarius-scheme
  (package
    (name "sagittarius-scheme")
    (version "0.9.11")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
	     "https://bitbucket.org/ktakashi/sagittarius-scheme/downloads/sagittarius-"
	     version ".tar.gz"))
       (sha256
	(base32 "1iwkcdmq7h420jrwj74im6j8967vwh2m89y4r4516c1cdw8pb09c"))))
    (build-system cmake-build-system)
    (native-inputs (list pkg-config))
    (inputs (list libgc zlib libffi openssl-3.3))
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
	  (delete 'check))))
    (synopsis "R6RS/R7RS Scheme system.")
    (description "Sagittarius is a R6RS/R7RS scheme implementation with features like built-in CLOS, Common Lisp like reader macro, cryptographic libraries, etc.")
    (home-page "https://ktakashi.github.io/")
    (license license:bsd-2)))
