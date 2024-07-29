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
