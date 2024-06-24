(define-module (abbe packages kakoune)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix licenses)
  #:use-module (guix build-system copy)
  #:use-module (gnu packages text-editors))

(define-public kak-lsp-17-0-1
  (package
   (name "kak-lsp")
   (version "17.0.1")
   (source (origin
            (method url-fetch/tarbomb)
            (uri (string-append
                  "https://github.com/kakoune-lsp/kakoune-lsp/releases/download/v"
                  version
                  "/kakoune-lsp-v"
                  version
                  "-x86_64-unknown-linux-musl.tar.gz"))
            (sha256
             (base32 "1pqii40g3rxva0kxi3rgq3a914s3kx82s59ypchw2a5asgwp2yaa"))))
   (build-system copy-build-system)
   (arguments
    '(#:install-plan
      (list
       (list "kak-lsp" "bin/")
       (list "kak-lsp.toml" "etc/kak-lsp/"))))
   (synopsis "Kakoune Language Server Protocol Client")
   (description "Kakoune Language Server Protocol Client")
   (home-page "https://github.com/kakoune-lsp/kakoune-lsp")
   (license unlicense)))
