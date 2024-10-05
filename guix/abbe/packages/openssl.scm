(define-module (abbe packages openssl)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix base16)
  #:use-module (gnu packages tls)
  #:export (openssl-3.3))

(define openssl-3.3
  (package/inherit openssl-3.0
    (version "3.3.2")
    (source (origin
              (inherit (package-source openssl-3.0))
              (uri
               (string-append "https://github.com/openssl/openssl/releases/download/openssl-"
                              version "/openssl-" version ".tar.gz"))
              (sha256
               (base16-string->bytevector "2e8a40b01979afe8be0bbfb3de5dc1c6709fedb46d6c89c10da114ab5fc3d281"))))))
