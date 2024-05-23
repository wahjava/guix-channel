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
  (define (fossil-patch name from to hash)
    (origin (method url-fetch)
	    (uri (string-append "https://fossil-scm.org/home/vpatch?from=" from
				"&to=" to))
	    (sha256 (base32 hash))
	    (file-name name)))
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
	       (base32 "0k1gjvxbvvs98yspnf7nj6rb0c7yf60savq7jywbkgimd0bkrkcm"))
	      (patch-flags '("-p0"))
	      (patches (list
			(fossil-patch "fossil-disable-tests.patch"
				      "8be0372c10510437"
				      "5ad708085a90365f"
				      "1b4svsrz7cr1zi6qfpavj3ddm6dr0966jbgkbvgjz79ljqmpiasf")
			(fossil-patch "fossil-fix-json-test.patch"
				      "fb4e90b662803e47"
				      "17c01c549e73c6b8"
				      "12gjzyxs22g9grv7qbgp9jg133bgcsj74621s05fk82j3fc7z59g")
			(fossil-patch "fossil-comment-utf-tests.patch"
				      "5ad708085a90365f"
				      "fb4e90b662803e47"
				      "05h2mb6g0840yq74x1cdj95jmqb95i75h6g5v0rzqdc994b96cd4")))))
    (inputs (modify-inputs (package-inputs fossil)
	      (delete "sqlite")
	      (append sqlite-next)))))
