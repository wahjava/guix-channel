(define-module (abbe packages gitui)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix build-system copy)
  #:use-module (guix download)
  #:use-module (ice-9 match)
  #:use-module (guix licenses))

(define (gitui-hash)
  (match (%current-system)
    ("aarch64-linux" "19n619wfsa52ny7xh9hv6c6nafh7i61yq8jz0jjp8dvdpdkdj20g")
    ("x86_64-linux" "1pz27y4c1hxpymd982laim2djnd92c2gc03sw88fg2c7v46mgacm")))

(define (gitui-system)
  (match (%current-system)
    ("aarch64-linux" "linux-aarch64")
    ("x86_64-linux"  "linux-x86_64")))

(define-public gitui
  (package
   (name "gitui")
   (version "0.26.3")
   (source (origin
            (method url-fetch)
            (uri 
             (string-append
              "https://github.com/extrawurst/gitui/releases/download/v"
              version
              "/gitui-"
              (gitui-system)
              ".tar.gz"))
            (sha256 (base32 (gitui-hash)))))
   (build-system copy-build-system)
   (arguments
    `(#:install-plan
      ,#~(list
          (list "gitui" "bin/gitui"))))
   (synopsis "Blazing 💥 fast terminal-ui for git written in rust 🦀")
   (description "GitUI provides you with the comfort of a git GUI but right in your terminal")
   (home-page "https://github.com/extrawurst/gitui")
   (license expat)))
