(define-module (abbe packages zoxide)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (nonguix build-system binary)
  #:use-module (guix download)
  #:use-module (ice-9 match)
  #:use-module (guix licenses))

(define (zoxide-arch system)
  (match system
    ("aarch64-linux" "aarch64")
    ("x86_64-linux" "x86_64")))

(define (zoxide-url version system)
  (let ([system (zoxide-arch system)])
    (string-append
     "https://github.com/ajeetdsouza/zoxide/releases/download/v" version "/zoxide-" version "-" system "-unknown-linux-musl.tar.gz")))

(define (zoxide-hash system)
  (match system
    ("aarch64-linux" "0czil9gll1mgcwbkls1hwd2kf64mbrfmyl327jw3v3xmhyw97pv5")
    ("x86_64-linux" "182m48gv7w271qynq4kb28hd042qd7nka7ihfdf9lbr5b88jrhpv")))

(define-public zoxide
  (package
   (name "zoxide")
   (version "0.9.4")
   (source (origin
            (method url-fetch/tarbomb)
            (uri (zoxide-url version (%current-system)))
            (sha256 (base32 (zoxide-hash (%current-system))))))
   (build-system binary-build-system)
   (arguments
    '(#:install-plan
      '(("zoxide" "bin/zoxide")
        ("man" "share/man")
        ("completions/_zoxide" "share/zsh/site-functions/")
        ("completions/zoxide.fish" "share/fish/vendor_completions.d/")
        ("completions/zoxide.bash" "share/bash-completion/completions/"))))

   (synopsis "A smarter cd command. Supports all major shells.")
   (description "zoxide is a smarter cd command, inspired by z and autojump. It remembers which directories you use most frequently, so you can \"jump\" to them in just a few keystrokes. zoxide works on all major shells.
\"")
   (home-page "https://github.com/ajeetdsouza/zoxide")
   (license expat)))
