(define-module (abbe packages git)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages version-control)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module ((guix licenses) :prefix license:))

(define-public git-2-46-0
  (let ((version "2.46.0"))
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
      (arguments
       (substitute-keyword-arguments (package-arguments git)
         ((#:phases phases '%standard-phases)
          #~(modify-phases #$phases
              (replace 'install-man-pages
                (lambda _
                  (let ((man (string-append #$output "/share/man")))
                    (mkdir-p man)
                    (with-directory-excursion man
                      (invoke
                       "tar" "xvf"
                       #$(origin
                           (method url-fetch)
                           (uri (string-append
                                 "mirror://kernel.org/software/scm/git/"
                                 "git-manpages-" (package-version this-package)
                                 ".tar.xz"))
                           (sha256 (base32 "1lvvhzypllbyd8j6m0p9qgd3gqg10gch9s7lqif8vr9n80fqn4fw")))))))))))))))
