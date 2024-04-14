(define-module (abbe packages fossil)
  #:use-module (guix packages)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages sqlite)
  #:use-module (guix download))

(define-public fossil-2-23
  (package
    (inherit fossil)
    (name "fossil-2-23")
    (version "2.23")
    (source (origin
              (method url-fetch)
              (uri
	       (string-append
		"https://fossil-scm.org/home/tarball/47362306a7dd7c6fc3cab77cebe5d25469b0a9448479d9718eb5c49c8337b29/fossil-src-" version ".tar.gz"))
              (sha256
               (base32 "1r1kabvmlhc0qgyq8g9zhq8i0123x9dba9b71j4xc71k14kfqjm9"))))
    (inputs (modify-inputs (package-inputs fossil)
	      (delete "sqlite")
	      (append sqlite-next)))))
