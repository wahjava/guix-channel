(define-module (abbe packages got)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages libbsd)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages base))

(define-public got
  (package
   (name "got")
   (version "0.100")
   (source (origin
            (method url-fetch)
            (uri (string-append "https://gameoftrees.org/releases/portable/got-portable-"
                                version ".tar.gz"))
            (sha256
             (base32
              "04jhaqzskr26akmy963yc8gaw1pqbsxhgsxzd0yrssgzcwh8lfpw"))))
   (inputs
    `(("libevent" ,libevent)
      ("libuuid" ,util-linux "lib")
      ("zlib" ,zlib)
      ("libressl" ,libressl)
      ("libmd" ,libmd)
      ("libbsd" ,libbsd)
      ("ncurses" ,ncurses)))
   (native-inputs
    (list pkg-config))
   (arguments
     ;; disable runpath validation, courtesy: libbsd's special
     ;; treatment of libmd
     (list #:validate-runpath? #f))
   (build-system gnu-build-system)
   (synopsis "Distributed version control system")
   (description
     "Game of Trees (Got) is a version control system which prioritizes ease of use
and simplicity over flexibility.")
    (license license:bsd-3)
    (home-page "https://gameoftrees.org/")))

