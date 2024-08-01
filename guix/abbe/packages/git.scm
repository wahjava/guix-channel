(define-module (abbe packages git)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages version-control)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module ((guix licenses) :prefix license:))

(define-public git-2-46-0
  (let* ((version "2.46.0")
	 (git-manpages (origin
			  (method url-fetch)
			  (uri (string-append
				"mirror://kernel.org/software/scm/git/git-manpages-"
				version ".tar.xz"))
			  (sha256
			   (base32
			    "1lvvhzypllbyd8j6m0p9qgd3gqg10gch9s7lqif8vr9n80fqn4fw")))))
    (package
      (inherit git)
      (name "git")
      (version version)
      (source (origin
		(method url-fetch)
		(uri (string-append "mirror://kernel.org/software/scm/git/git-"
                                    version ".tar.xz"))
		(sha256
		 (base32
		  "15bzq9m6c033qiz5q5gw1nqw4m452vvqax30wbms6z4bl9i384kz"))))
      (native-inputs (modify-inputs (package-native-inputs git)
		       (replace "git-manpages" git-manpages))))))
