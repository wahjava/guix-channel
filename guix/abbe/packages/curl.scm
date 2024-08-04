(define-module (abbe packages curl)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages web)
  #:use-module (gnu packages curl))

(define-public curl-http3
  (package/inherit curl-ssh
                   (arguments (substitute-keyword-arguments (package-arguments curl-ssh)
                                ((#:configure-flags flags)
                                 #~(cons* "--with-nghttp3"
                                          "--with-ngtcp2"
                                          #$flags))))
                   (inputs (modify-inputs (package-inputs curl-ssh)
                                          (prepend nghttp3 ngtcp2)))
                   (properties `((hidden? . #t)))))
