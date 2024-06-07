(define-module (abbe packages fzf)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (nonguix build-system binary)
  #:use-module (guix download)
  #:use-module (ice-9 match)
  #:use-module (guix licenses))

(define (fzf-arch system)
  (match system
    ("aarch64-linux" "linux_arm64")
    ("x86_64-linux" "linux_amd64")))

(define (fzf-url version system)
  (let ([system (fzf-arch system)])
    (string-append
      "https://github.com/junegunn/fzf/releases/download/" version "/fzf-" version "-" system ".tar.gz")))

(define (fzf-hash system)
  (match system
    ("x86_64-linux" "15ipbvq0d84qjimlacha95zs7y79q99pwlr13lcxqrwr631n69mg")
    ("aarch64-linux" "0fx4jjfkf50m6c2rf87kzybpdsnr5yssdzv5rg5laga3ns6wzd25")))

(define-public fzf
  (package
   (name "fzf")
   (version "0.53.0")
   (source (origin
            (method url-fetch/tarbomb)
            (uri (fzf-url version (%current-system)))
            (sha256 (base32 (fzf-hash (%current-system))))))
   (build-system binary-build-system)
   (arguments
    '(#:install-plan
      '(("fzf" "bin/fzf"))))

   (synopsis "A command-line fuzzy finder")
   (description "A command-line fuzzy finder")
   (home-page "https://junegunn.github.io/fzf/")
   (license expat)))
