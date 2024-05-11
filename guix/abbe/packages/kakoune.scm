(define-module (abbe packages kakoune)
  #:use-module (guix download)
  #:use-module (gnu packages text-editors))

(define-public kakoune-2024-05-09
  (package
   (inherit kakoune)
   (version "2024.05.09")
   (source
    (origin
     (method url-fetch)
     (uri (string-append "https://github.com/mawww/kakoune/"
                         "releases/download/v" version "/"
                         "kakoune-" version ".tar.bz2"))
     (sha256
      (base32 "0ad22syhsd921rnfpmkvyh37am3ni443g1f3jc2hqndgsggvv411"))))))
