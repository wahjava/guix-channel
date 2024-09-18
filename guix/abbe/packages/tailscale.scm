(define-module (abbe packages tailscale)
  #:use-module (guix packages)
  #:use-module (gnu packages linux)
  #:use-module (guix gexp)
  #:use-module (guix build-system copy)
  #:use-module (guix download)
  #:use-module ((guix licenses) :prefix license:)
  #:use-module (ice-9 match))

(define (tailscale-arch system)
  (match system
    ("aarch64-linux" "arm64")
    ("x86_64-linux" "amd64")
    ("i686-linux" "386")))

(define (tailscale-url version system)
  (let ([system (tailscale-arch system)])
    (string-append "https://pkgs.tailscale.com/stable/tailscale_" version
                   "_" system ".tgz")))

(define (tailscale-hash system)
  (match system
    ("aarch64-linux" "089y6wg8m96kxw0cabdbf1wxl0q8cv1gkc44x6bxljwfmfsid8x7")
    ("x86_64-linux" "12196z8dhdzxq8mjj0rlvhr6wchmi6z33ym36yqqrp4m52xjz7q5")
    ("i686-linux" "146vfnj65dd7q7vvh07bis5b26rs3jw385m9wr11jf12q8ad4lsa")))

(define-public tailscale
  (package
   (name "tailscale")
   (version "1.74.1")
   (source (origin
            (method url-fetch/tarbomb)
            (uri (tailscale-url version (%current-system)))
            (sha256 (base32 (tailscale-hash (%current-system))))))
   (build-system copy-build-system)
   (arguments
    (list
     #:install-plan
     #~`((,(string-append "tailscale_" #$version "_" #$(tailscale-arch (%current-system)) "/") "bin/"))))
   (propagated-inputs
                                        ; iptables is required for setting up routing
    (list iptables))
   (synopsis "Tailscale connects your team's devices and development environments for easy access to remote resources.")
   (description
    "Tailscale is a zero config VPN for building secure networks. Install on any device in minutes. Remote access from any network or physical location.")
   (home-page "https://tailscale.com/")
   (license license:bsd-3)))
