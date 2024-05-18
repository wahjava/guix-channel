(define-module (abbe packages kakoune)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (gnu packages text-editors))

(define-public kakoune-2024-05-18
  (package
   (inherit kakoune)
   (name "kakoune-2024-05-18")
   (version "2024.05.18")
   (source
    (origin
     (method url-fetch)
     (uri (string-append "https://github.com/mawww/kakoune/"
                         "releases/download/v" version "/"
                         "kakoune-" version ".tar.bz2"))
     (sha256
      (base32 "1ymr1jpdnd5wj6npzi8bgfd30d0j885sfnhl236rn7fjc4pars6s"))))))
