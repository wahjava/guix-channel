(define-module (abbe packages gopass)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (nonguix build-system binary)
  #:use-module (guix download)
  #:use-module (ice-9 match)
  #:use-module (guix licenses))

(define (gopass-arch system)
  (match system
    ("aarch64-linux" "linux-arm64")
    ("x86_64-linux" "linux-amd64")))

(define (gopass-url version system)
  (let ([system (gopass-arch system)])
    (string-append
     "https://github.com/gopasspw/gopass/releases/download/v" version "/gopass-" version "-" system ".tar.gz")))

(define (gopass-hash system)
  (match system
    ("aarch64-linux" "0gaqk8z4sb4gbnicmhznqmar1b7r39vjvb2bddnpn1w1dwxyhp33")
    ("x86_64-linux" "0gssd4yqz1kbf8yf723b6l03lr0kfm1f0qk0p1krvf5fxmq8xabs")))

(define-public gopass
  (package
   (name "gopass")
   (version "1.15.14")
   (source (origin
            (method url-fetch/tarbomb)
            (uri (gopass-url version (%current-system)))
            (sha256 (base32 (gopass-hash (%current-system))))))
   (build-system binary-build-system)
   (arguments
    '(#:install-plan
      '(("gopass" "bin/gopass")
	("zsh.completion" "share/zsh/site-functions/_gopass")
	("bash.completion" "share/bash-completion/completions/gopass")
	("fish.completion" "share/fish/vendor_completions.d/gopass.fish"))))

   (synopsis "The slightly more awesome standard unix password manager for teams")
   (description "The slightly more awesome standard unix password manager for teams")
   (home-page "https://gopass.pw")
   (license expat)))
