(define-module (abbe packages mcfly)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (nonguix build-system binary)
  #:use-module (guix download)
  #:use-module (ice-9 match)
  #:use-module (guix licenses))

(define (mcfly-arch system)
  (match system
    ("aarch64-linux" "aarch64")
    ("x86_64-linux" "x86_64")))

(define (mcfly-url version system)
  (let ([system (mcfly-arch system)])
    (string-append
     "https://github.com/cantino/mcfly/releases/download/v" version "/mcfly-v" version "-" system "-unknown-linux-musl.tar.gz")))

(define (mcfly-hash system)
  (match system
    ("aarch64-linux" "112jxqf75izvh04yk5wd4dy6mfi1ws55kqzrhw365d4qk1bb9lh5")
    ("x86_64-linux" "0fvqr1fyskiajbp401m3rqlm3hxlik36r36kj3cc14y1wkcb8v5x")))

(define-public mcfly
  (package
   (name "mcfly")
   (version "0.9.0")
   (source (origin
            (method url-fetch/tarbomb)
            (uri (mcfly-url version (%current-system)))
            (sha256 (base32 (mcfly-hash (%current-system))))))
   (build-system binary-build-system)
   (arguments
    '(#:install-plan
      '(("mcfly" "bin/mcfly"))))

   (synopsis "Fly through your shell history")
   (description "Fly through your shell history")
   (home-page "https://github.com/cantino/mcfly")
   (license expat)))
