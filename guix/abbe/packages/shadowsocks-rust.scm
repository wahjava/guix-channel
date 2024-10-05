(define-module (abbe packages shadowsocks-rust)
  #:use-module (guix packages)
  #:use-module (abbe build-system nix-rust)
  #:use-module (abbe build-system nix-go)
  #:use-module (ice-9 match)
  #:use-module (guix base16)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix git-download))

(define %empty-hash (make-string 52 #\0))

(define-public shadowsocks-v2ray-plugin
  (package
    (name "shadowsocks-v2ray-plugin")
    (version "1.3.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/shadowsocks/v2ray-plugin")
                    (commit (string-append "v" version))))
              (sha256 (base32 "0ncx3rlch00hf48l236sj0v73cmmyc4dbbx34zpnblknk9s0csxh"))))
    (build-system nix-go-build-system)
    (arguments
     `(#:vendor-hash "17xiadakhvskfm42ck9zlzjwg5lp6pp5h3d0fqdsf1bq8vvknvxx"))

    (home-page "https://shadowsocks.org/")
    (synopsis "A SIP003 plugin based on v2ray")
    (description "Yet another SIP003 plugin for shadowsocks, based on v2ray")
    (license license:expat)))

(define-public shadowsocks-rust
  (package
    (name "shadowsocks-rust")
    (version "1.21.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/shadowsocks/shadowsocks-rust")
                    (commit (string-append "v" version))))
              (sha256 (base32 "0db96y6132pr82zvvb0qwyrjhyyzgqajk9cv2iwsfw3a5iznx107"))))
    (build-system nix-rust-build-system)
    (arguments
     `(#:vendor-hash "1sgh7hlpl72wmdzbagkbg47hbbcaav43mmv2fry6myvv1bpwxkkj"))
    (home-page "https://shadowsocks.org/")
    (synopsis "Rust port of shadowsocks")
    (description "shadowsocks is a fast tunnel proxy that helps you bypass firewalls.")
    (license license:expat)))
