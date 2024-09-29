(define-module (abbe packages elvish)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (abbe packages go)
  #:use-module (abbe build-system nix-go)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:))

(define-public elvish
  (package
    (name "elvish")
    (version "0.21.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/elves/elvish")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1yrah4xbm8jsmr578676bd0yq1n3dc6ahs0hav6csnl7739jpags"))))
    (build-system nix-go-build-system)
    (arguments
     (list
      #:go go-122
      #:vendor-hash "1fpqzpgivivh9j8qdbq2xk1jf64vx5zdxbxmf2wwiv7xrczzadaj"
      #:sub-packages '("./cmd/elvish")))

    (home-page "https://elv.sh")
    (synopsis "Elvish shell")
    (description "(Chat rooms are all bridged together thanks to
@@url{https://matrix.org,Matrix}.).")
    (license license:bsd-2)))
