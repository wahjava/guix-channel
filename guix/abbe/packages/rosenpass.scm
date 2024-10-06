(define-module (abbe packages rosenpass)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix build utils)
  #:use-module (abbe build-system nix-rust)
  #:use-module (guix git-download)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages gawk)
  #:use-module ((guix licenses) :prefix license:))

(define-public rosenpass
  (package
    (name "rosenpass")
    (version "0.2.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/rosenpass/rosenpass")
                    (commit (string-append "v" version))))
              (sha256 (base32 "0gkc4i8s5fggcq8mp5afrqx3b18vg9pa6dfkaya5b44kdhl1w0kx"))))
    (build-system nix-rust-build-system)
    (native-inputs (list pkg-config cmake))
    (inputs (list libsodium clang))
    (propagated-inputs (list gawk))
    (arguments
     `(#:vendor-hash "1skvkrygk9xay1f0qkljirdwxymjg11hz1xzqv6c33vlckxapakg"
       #:cargo-install-paths ("./rosenpass")
       #:env-vars
       (list (cons "LIBCLANG_PATH" (string-append (assoc-ref %build-inputs "clang") "/lib")))
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-manpages-et-al
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (mandir (string-append out "/share/man/man1")))
               (install-file "doc/rosenpass.1" mandir)
               (install-file "doc/rp.1" mandir)
               (install-file "rp" (string-append out "/bin"))))))))
    (home-page "https://rosenpass.eu/")
    (synopsis "Build post-quantum-secure VPNs with WireGuard!")
    (description "Rosenpass is free and open-source software based on the latest research in the field of cryptography. It is intended to be used with WireGuard VPN, but can work with all software that uses pre-shared keys. It uses two cryptographic methods (Classic McEliece and Kyber) to secure systems against attacks with quantum computers.")
    (license (list license:expat license:asl2.0))))

