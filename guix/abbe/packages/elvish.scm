(define-module (abbe packages elvish)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (gnu packages base)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages golang-xyz)
  #:use-module (gnu packages golang-build)
  #:use-module (gnu packages golang-web)
  #:use-module (guix build-system go)
  #:use-module (guix gexp)
  #:use-module (ice-9 match)
  #:use-module (guix build-system copy)
  #:use-module ((guix licenses) #:prefix license:))

(define (elvish-url version)
  (match (%current-system)
    ("x86_64-linux" (string-append "https://dl.elv.sh/linux-amd64/elvish-v" version ".tar.gz"))
    ("aarch64-linux" (string-append "https://dl.elv.sh/linux-arm64/elvish-v" version ".tar.gz"))))

(define (elvish-hash version)
  (match (%current-system)
    ("x86_64-linux" "0wkmiyspfdy8ks5zn23hq84a6xya4fnp06hcvlxispfsvld9d3yp")
    ("aarch64-linux" "1vip0jkw6h3n6kj6vsfqc1db4lkrz3dz0wxs8i9mdrfgvgdwjs2k")))

(define-public elvish-bin
  (package
    (name "elvish-bin")
    (version "0.21.0")
    (source (origin
              (method url-fetch/tarbomb)
              (uri (elvish-url version))
              (sha256 (base32 (elvish-hash version)))))
    (build-system copy-build-system)
    (arguments
     `(#:install-plan
       '(("elvish" "bin/elvish"))))
    (home-page "https://elv.sh")
    (synopsis "Interactive shell, and a scripting language.")
    (description "A shell with useful interactive features built-in.")
    (license license:bsd-2)))

(define-public go-pkg-nimblebun-works-go-lsp
  (package
    (name "go-pkg-nimblebun-works-go-lsp")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/nimblebun/go-lsp")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "14hdbk0h85930phnsih5k33dj2qx9b3j4vvsf24g6v3qqjvbp54q"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "pkg.nimblebun.works/go-lsp"))
    (home-page "https://pkg.nimblebun.works/go-lsp")
    (synopsis "Language Server Protocol types for Go")
    (description
     "Package lsp contains type definitions and documentation for the Language Server
Protocol.")
    (license license:expat)))

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
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "src.elv.sh/cmd/elvish"
      #:unpack-path "src.elv.sh"))
    (propagated-inputs `(("go-pkg-nimblebun-works-go-lsp" ,go-pkg-nimblebun-works-go-lsp)
                         ("go-golang-org-x-sys" ,go-golang-org-x-sys)
                         ("go-golang-org-x-sync" ,go-golang-org-x-sync)
                         ("go-go-etcd-io-bbolt" ,go-go-etcd-io-bbolt)
                         ("go-github-com-sourcegraph-jsonrpc2" ,go-github-com-sourcegraph-jsonrpc2)
                         ("go-github-com-mattn-go-isatty" ,go-github-com-mattn-go-isatty)
                         ("go-github-com-google-go-cmp" ,go-github-com-google-go-cmp)
                         ("go-github-com-creack-pty" ,go-github-com-creack-pty)))
    (home-page "https://src.elv.sh")
    (synopsis "Elvish")
    (description "(Chat rooms are all bridged together thanks to
@@url{https://matrix.org,Matrix}.).")
    (license license:bsd-2)))
