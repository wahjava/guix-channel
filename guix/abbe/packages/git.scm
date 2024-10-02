(define-module (abbe packages git)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages version-control)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module ((guix licenses) :prefix license:))

(define-public git/update
  (let ((version "2.46.2"))
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
                  "18rcmvximgyg3v1a9papi9djfamiak0ys5cmgx7ll29nhp3a3s2y"))))
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
                           (sha256 (base32 "1bn2jifax2w0q4p67qhnb3fwsx9qvqpa0h2c857zs8mnn2cmp6gy")))))))))))))))
