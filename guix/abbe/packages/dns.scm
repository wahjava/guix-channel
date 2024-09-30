(define-module (abbe packages dns)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix base16)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages dns)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages sphinx))

(define-public knot-resolver-6
  (package/inherit knot-resolver
    (version "6.0.8")
    (native-inputs
      (modify-inputs (package-native-inputs knot-resolver)
        (delete "python-sphinx" "python-sphinx-rtd-theme"
                "texinfo" "python-breathe" "doxygen")))
    (outputs '("out"))
    (arguments
     (substitute-keyword-arguments (strip-keyword-arguments  '(#:configure-flags) (package-arguments knot-resolver))
       ((#:phases phases '())
        #~(modify-phases #$phases
            (delete 'build-doc)
            (delete 'move-doc)))))
    (source (origin
              (method url-fetch)
              (uri (string-append "https://secure.nic.cz/files/knot-resolver/"
                                  "knot-resolver-" version ".tar.xz"))
              (sha256
               (base16-string->bytevector
                "ac12f000ab599fd2223e418611e77746da717c6ba27654661fa36c847185a266"))))))
