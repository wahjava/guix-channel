(define-module (abbe packages shadowsocks-rust)
  #:use-module (guix packages)
  #:use-module (ice-9 match)
  #:use-module (guix base16)
  #:use-module (guix build-system copy)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download))

(define (shadowsocks-rust-hash)
  (match (%current-system)
         ("aarch64-linux" "80967bf5852b9215b865849c1ecbcf3d2766aaa4e49cf8a0b35cb3d7760e72a3")
         ("x86_64-linux"  "c524f3c3c6a45c91c8eba0a68c6543862586e41540e260c17cbdb046da895621")))

(define (shadowsocks-rust-url version)
  (match (%current-system)
         ("aarch64-linux" (string-append "https://github.com/shadowsocks/shadowsocks-rust/releases/download/v" version
                                         "/shadowsocks-v" version ".aarch64-unknown-linux-musl.tar.xz"))
         ("x86_64-linux" (string-append "https://github.com/shadowsocks/shadowsocks-rust/releases/download/v" version
                                        "/shadowsocks-v" version ".x86_64-unknown-linux-musl.tar.xz"))))
          
           
(define-public shadowsocks-rust
               (package
                 (name "shadowsocks-rust")
                 (version "1.20.3")
                 (source (origin
                           (method url-fetch)
                           (uri (shadowsocks-rust-url version))
                           (sha256 (base16-string->bytevector (shadowsocks-rust-hash)))))
                 (build-system copy-build-system)
                 (arguments
                   `(#:install-plan
                     '(("ssurl" "bin/")
                       ("sslocal" "bin/")
                       ("ssservice" "bin/")
                       ("ssmanager" "bin/")
                       ("ssserver" "bin/"))))

                 (home-page "https://shadowsocks.org/")
                 (synopsis "Rust port of shadowsocks")
                 (description "shadowsocks is a fast tunnel proxy that helps you bypass firewalls.")
                 (license license:expat)))
