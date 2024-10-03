(define-module (abbe packages gitui)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (abbe build-system nix-rust)
  #:use-module (guix git-download)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages ssh)
;  #:use-module (guix build-utils)
  #:use-module (gnu packages pkg-config)
  #:use-module ((guix licenses) #:prefix license:))

(define-public gitui
  (package
   (name "gitui")
   (version "0.26.3")
   (source
    (origin (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/extrawurst/gitui")
                  (commit (string-append "v" version))))
            (sha256 (base32 "0afshcj1zspkn4sa92danm9z1ssx7vgnry37l9vdm8xy60mbwz4g"))))
   (inputs (list openssl libssh2))
   (native-inputs
     (list (list "pkg-config" pkg-config)))
   (build-system nix-rust-build-system)
   (arguments
    `(#:vendor-hash "1xhznj6cw7nil26qfxx7pawa7vfllhglgbp2jknnc22y3jznm0xv"
      #:env-vars
      (list '("OPENSSL_NO_VENDOR" . "yes")
            '("LIBSSH2_SYS_USE_PKG_CONFIG" . "yes")
            '("BUILD_GIT_COMMIT_ID" . "95e1d4d4324bf1eab34f8100afc7f3ae7e435252"))))
   (synopsis "Blazing 💥 fast terminal-ui for git written in rust 🦀")
   (description "GitUI provides you with the comfort of a git GUI but right in your terminal")
   (home-page "https://github.com/extrawurst/gitui")
   (license license:expat)))
