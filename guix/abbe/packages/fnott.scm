(define-module (abbe packages fnott)
               #:use-module (guix packages)
               #:use-module (guix git-download)
               #:use-module (gnu packages wm))

(define-public fnott-1-7
               (package/inherit fnott
                                (name "fnott")
                                (version "1.7.0")
                                (source
                                  (origin (method git-fetch)
                                          (uri (git-reference
                                                 (url "https://codeberg.org/dnkl/fnott")
                                                 (commit version)))
                                          (file-name (git-file-name name version))
                                          (sha256 (base32 "0x3wfqkkz6lf9w00nl5mchcw04gx884jxxz6f7z3fxpsjx582nr9"))))))
