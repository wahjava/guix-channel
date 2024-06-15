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
    ("aarch64-linux" "0d8asxrhy5dywjvh374xdgah00g9vjmycgh8agw6412f89yrjk6j")
    ("x86_64-linux" "1qrprimabssr1y5bgsxhkhhnc1g1jdfv9spzxvbbrgs1gh3c7v8q")
    ("i686-linux" "0jwkj1nmia2wciyib8fhgv5qjjysmhhlkyy3vqy7j2a8mj1pw21z")))

(define-public tailscale
  (package
   (name "tailscale")
   (version "1.68.1")
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
