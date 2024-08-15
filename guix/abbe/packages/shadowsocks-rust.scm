(define-module (abbe packages shadowsocks-rust)
  #:use-module (guix packages)
  #:use-module (ice-9 match)
  #:use-module (guix base16)
  #:use-module (guix build-system copy)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download))

(define (shadowsocks-rust-hash)
  (match (%current-system)
         ("aarch64-linux" "e6c4a001b1e4200d9c43ce50acfe57cbbb4a89b69bf16feb42085e0fa49c4344")
         ("x86_64-linux"  "bc8a262da2dfeedcec2dd38f9c82f8431f3c134d6959bea7bb6d624f4640f81e")))

(define (shadowsocks-rust-url version)
  (match (%current-system)
         ("aarch64-linux" (string-append "https://github.com/shadowsocks/shadowsocks-rust/releases/download/v" version
                                         "/shadowsocks-v" version ".aarch64-unknown-linux-musl.tar.xz"))
         ("x86_64-linux" (string-append "https://github.com/shadowsocks/shadowsocks-rust/releases/download/v" version
                                        "/shadowsocks-v" version ".x86_64-unknown-linux-musl.tar.xz"))))

(define (shadowsocks-v2ray-url version)
  (match (%current-system)
         ("aarch64-linux" (string-append "https://github.com/shadowsocks/v2ray-plugin/releases/download/v" version
                                         "/v2ray-plugin-linux-arm64-v" version ".tar.gz"))
         ("x86_64-linux" (string-append "https://github.com/shadowsocks/v2ray-plugin/releases/download/v" version
                                        "/v2ray-plugin-linux-amd64-v" version ".tar.gz"))))

(define (shadowsocks-v2ray-hash)
  (match (%current-system)
         ("aarch64-linux" "1hzycqgcwifi4jih7w778135n759mn0x42bqa7ncsjfjvdy4c1zg")
         ("x86_64-linux"  "06jlxs9kjprfhg1rzh2v2f2my83pf8da18hsi07j72xr6m152y5m")))

(define-public shadowsocks-v2ray-plugin
               (package
                 (name "shadowsocks-v2ray-plugin")
                 (version "1.3.2")
                 (source (origin
                           (method url-fetch/tarbomb)
                           (uri (shadowsocks-v2ray-url version))
                           (sha256 (base32 (shadowsocks-v2ray-hash)))))
                 (build-system copy-build-system)
                 (arguments
                   `(#:install-plan
                     '((,(match (%current-system)
                                 ("aarch64-linux" "v2ray-plugin_linux_arm64")
                                 ("x86_64-linux" "v2ray-plugin_linux_amd64")) "bin/v2ray-plugin"))))

                 (home-page "https://shadowsocks.org/")
                 (synopsis "A SIP003 plugin based on v2ray")
                 (description "Yet another SIP003 plugin for shadowsocks, based on v2ray")
                 (license license:expat)))

(define-public shadowsocks-rust
               (package
                 (name "shadowsocks-rust")
                 (version "1.20.4")
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
