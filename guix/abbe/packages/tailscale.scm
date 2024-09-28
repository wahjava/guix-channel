(define-module (abbe packages tailscale)
  #:use-module (guix packages)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages golang)
  #:use-module (guix gexp)
  #:use-module (abbe build-system nix-go)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) :prefix license:))

(define-public tailscale
  (package
    (name "tailscale")
    (version "1.74.1")
    (source (origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/tailscale/tailscale")
                   (commit (string-append "v" version))))
             (file-name (git-file-name name version))
             (sha256 (base32 "0ncck013rzbrzcbpya1fq41jrgzxw22pps77l9kb7kx06as8bggb"))))
    (build-system nix-go-build-system)
    (home-page "https://tailscale.com")
    (propagated-inputs
     (list iptables))
    (synopsis "Tailscale connects your team's devices and development environments for easy access to remote resources.")
    (description
     "Tailscale is a zero config VPN for building secure networks. Install on any device in minutes. Remote access from any network or physical location.")
    (license license:bsd-3)
    (arguments
     `(#:go ,go-1.23
       #:sub-packages '("./cmd/tailscaled")
       #:tags '("ts_include_cli")
       #:vendor-hash "1wfsk0z43aqi1i2ymhlhzziqc2a96bf2sydg8ppkafw6rl32148w"
       #:ldflags (list "-w" "-s"
                  (string-append "-X tailscale.com/version.longStamp=" ,(package-version this-package))
                  (string-append "-X tailscale.com/version.shortStamp=" ,(package-version this-package)))
       #:phases
       ,#~(modify-phases %standard-phases
           (add-after 'install 'symlink-tailscale
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (bin (string-append out "/bin")))
                 (symlink "tailscaled" (string-append bin "/tailscale"))))))))))
