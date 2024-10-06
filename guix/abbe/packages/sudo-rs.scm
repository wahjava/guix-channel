(define-module (abbe packages sudo-rs)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix build utils)
  #:use-module (abbe build-system nix-rust)
  #:use-module (guix git-download)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages linux)
  #:use-module ((guix licenses) :prefix license:))

(define-public sudo-rs
  (package
    (name "sudo-rs")
    (version "0.2.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/trifectatechfoundation/sudo-rs")
                    (commit (string-append "v" version))))
              (sha256 (base32 "1qcb3d3rggsfrynqdb52kzfqshy94ra9dcm1f59h5q1qv08s8wk9"))))
    (build-system nix-rust-build-system)
    (native-inputs (list pkg-config))
    (inputs (list linux-pam))
    (arguments
     `(#:vendor-hash "07d67ip7pm86p3d1pj0x0h5sw6b3ih9zjrgw3d5m5rgjm1vkdzpd"))
    (home-page "https://github.com/trifectatechfoundation/sudo-rs")
    (synopsis "A memory safe implementation of sudo, and su")
    (description "A safety oriented and memory safe implementation of sudo and su written in Rust.")
    (license (list license:expat license:asl2.0))))

