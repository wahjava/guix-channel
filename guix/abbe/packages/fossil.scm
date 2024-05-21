(define-module (abbe packages fossil)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages compression)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module ((guix licenses) :prefix license:))

(define-public fossil-2-24
  (package
    (inherit fossil)
    (name "fossil")
    (version "2.24")
    (source (origin
              (method url-fetch)
              (uri
	       (string-append
		"https://fossil-scm.org/home/tarball/version-" version "/fossil-" version ".tar.gz"))
              (sha256
               (base32 "0k1gjvxbvvs98yspnf7nj6rb0c7yf60savq7jywbkgimd0bkrkcm"))))
    (inputs (modify-inputs (package-inputs fossil)
	      (delete "sqlite")
	      (append sqlite-next)))
    (arguments
     (substitute-keyword-arguments (package-arguments fossil)
				   ((#:phases original-phases #~(list))
				    #~(modify-phases #$original-phases
						     (delete 'check)))))))
