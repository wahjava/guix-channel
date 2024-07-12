(define-module (abbe packages caddy)
  #:use-module (guix packages)
  #:use-module (gnu packages linux)
  #:use-module (guix gexp)
  #:use-module (guix build-system copy)
  #:use-module (guix download)
  #:use-module ((guix licenses) :prefix license:)
  #:use-module (ice-9 match))

(define (caddy-arch system)
  (match system
    ("aarch64-linux" "arm64")
    ("x86_64-linux" "amd64")))

(define (caddy-url version system)
  (let ([system (caddy-arch system)])
    (string-append
      "https://github.com/caddyserver/caddy/releases/download/v" version "/caddy_" version
      "_linux_" system ".tar.gz")))

(define (caddy-hash system)
  (match system
    ("aarch64-linux" "01dgn8l7ab7bycyxbnfwcsps7xxrdgmj7n66j1jqqrrxi0qyp8wk")
    ("x86_64-linux" "1j286xfxcrzvgn4k02nnq7n45wdsyz5c0phw6y7gi30kain31s57")))

(define-public caddy
  (package
   (name "caddy")
   (version "2.8.4")
   (source (origin
            (method url-fetch/tarbomb)
            (uri (caddy-url version (%current-system)))
            (sha256 (base32 (caddy-hash (%current-system))))))
   (build-system copy-build-system)
   (arguments
    (list
     #:install-plan
     #~'(("caddy" "bin/"))))
   (synopsis "Caddy is a web server")
   (description
    "Fast and extensible multi-platform HTTP/1-2-3 web server with automatic HTTPS")
   (home-page "https://caddyserver.com/")
   (license license:asl2.0)))
