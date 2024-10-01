(define-module (abbe packages aerc)
  #:use-module (guix download)
  #:use-module (gnu packages mail)
  #:use-module (guix utils)
  #:use-module (guix packages))

(define-public aerc/fixed
  (package-with-extra-patches
   aerc
   (list
    ;; patch from upstream to fix encoding problem with sending
    ;; gpg-signed messages
    ;; see https://todo.sr.ht/~rjarry/aerc/79
    (origin
      (method url-fetch)
      (uri "https://git.sr.ht/~rjarry/aerc/commit/7346d20.patch")
      (file-name "aerc-fix-gpg-signed-message-encoding.patch")
      (sha256 (base32
               "14avr323sr9qipf9d7npqwrzq37i9946z9m6jdkzi8n9rs6zwzq9"))))))
