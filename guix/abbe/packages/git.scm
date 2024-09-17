(define-module (abbe packages git)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages version-control)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module ((guix licenses) :prefix license:))

(define-public git-2-46-1
  (let ((version "2.46.1"))
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
		              "0hzmy1vhiz2w34bjdb633pw8ic0fhnl4100npjzcpd3apnwaz348"))))
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
                           (sha256 (base32 "0smz24yfdpczr2p03b4d6v3fn93g19gv9z8s0v4fjdx7w2xf8liw")))))))))))))))
