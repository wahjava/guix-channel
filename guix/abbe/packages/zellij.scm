(define-module (abbe packages zellij)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (nonguix build-system binary)
  #:use-module (guix download)
  #:use-module (ice-9 match)
  #:use-module (guix licenses))

(define (zellij-arch system)
  (match system
    ("aarch64-linux" "aarch64")
    ("x86_64-linux" "x86_64")))

(define (zellij-url version system)
  (let ([system (zellij-arch system)])
    (string-append
      "https://github.com/zellij-org/zellij/releases/download/v"
      version
      "/zellij-"
      system
      "-unknown-linux-musl.tar.gz")))

(define (zellij-hash system)
  (match system
    ("aarch64-linux" "0cfd3zjzlqsrxks53fh93zp4l60rk2lg15vhr0l0hcch95mxdw0d")
    ("x86_64-linux" "03rp4k9hw4f19i498qfy818qp2vi1ndlw0nfw0lxhbxvv7r3xzd9")))

(define-public zellij
  (package
   (name "zellij")
   (version "0.40.1")
   (source (origin
            (method url-fetch/tarbomb)
            (uri (zellij-url version (%current-system)))
            (sha256 (base32 (zellij-hash (%current-system))))))
   (build-system binary-build-system)
   (arguments
    '(#:install-plan
      '(("zellij" "bin/zellij"))))

   (synopsis "A terminal workspace with batteries included")
   (description "Zellij is a workspace aimed at developers, ops-oriented people and anyone who loves the terminal. Similar programs are sometimes called \"Terminal Multiplexers\".")
   (home-page "https://zellij.dev")
   (license expat)))
