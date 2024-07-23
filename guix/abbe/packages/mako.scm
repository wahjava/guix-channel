(define-module (abbe packages mako)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (gnu packages wm))

(define-public mako-1-9
  (package/inherit mako
    (version "1.9.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/emersion/mako")
                    (commit (string-append "v" version))))
              (file-name (git-file-name (package-name mako) version))
              (sha256
               (base32
                "0wcyhnpah1g5qpixfwlpybsjcl22iv39jrxlbi84xv2gfyi2vmj2"))))))
