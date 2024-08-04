(define-module (abbe packages curl)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages web)
  #:use-module (gnu packages curl))

(define-public curl-http3
  (package/inherit curl-ssh
     (name "curl")
     (version "8.9.1")
     (source (origin
               (method url-fetch)
               (uri (string-append "https://curl.se/download/curl-"
                                   version ".tar.xz"))
               (sha256
                (base32
                 "1r8171j91r9ac6byl5yp3mqwib7y5m1mvy2yfazvlnqx0p6gd4pj"))
               (patches (search-patches "curl-use-ssl-cert-env.patch"))))
     (arguments (substitute-keyword-arguments (package-arguments curl-ssh)
                  ((#:configure-flags flags)
                   #~(cons* "--with-nghttp3"
                            "--with-ngtcp2"
                            #$flags))))
     (inputs (modify-inputs (package-inputs curl-ssh)
                            (prepend nghttp3 ngtcp2)))
     (properties `((hidden? . #t)))))
