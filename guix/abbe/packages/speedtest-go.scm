(define-module (abbe packages speedtest-go)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (abbe build-system nix-go)
  #:use-module (guix git-download)
  #:use-module (guix licenses))

(define-public speedtest-go
  (package
    (name "speedtest-go")
    (version "1.7.9")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/showwin/speedtest-go")
                    (commit (string-append "v" version))))
              (sha256 (base32 "1a0saxg7mbklzn4sv29lyg6d4shg5x69qnpndj7gy2d59sbm8ms8"))))
    (build-system nix-go-build-system)
    (arguments
     `(#:vendor-hash "0l8ps3v5dy5p3lr6bmzcrhqcdsayd2sg95b967i19irfnrgq02n1"
       #:ldflags (list "-s" "-w" "-X"
                       ,(string-append  "main.version=" version)
                       "-X"
                       "main.commit=dd78c657d0f3"
                       "-X"
                       "main.date=1970-01-01T00:00:00Z")))
    (synopsis "CLI and Go API to Test Internet Speed using speedtest.net")
    (description "Full-featured Command Line Interface and pure Go API to Test Internet Speed using speedtest.net.")
    (home-page "https://github.com/showwin/speedtest-go")
    (license expat)))
