(define-module (abbe packages eza)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (abbe build-system nix-rust)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:))

(define-public eza
  (package
    (name "eza")
    (version "0.20.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/eza-community/eza")
                    (commit (string-append "v" version))))
              (sha256 (base32 "0nz6wcl5pvd3lihd5cc38r77jz466hsri0jwxsdzfzl538zwd6kl"))))
    (build-system nix-rust-build-system)
    (arguments
     `(#:vendor-hash "0l49n1s1ffivf0cbfhmcg4d591658gnglss8bbp1yimdf90w6fnq"))

    (synopsis "A modern alternative to ls.")
    (description "eza is a modern, maintained replacement for the venerable file-listing
command-line program \"ls\" that ships with Unix and Linux operating systems,
giving it more features and better defaults.")
    (home-page "https://eza.rocks/")
    (license license:expat)))
