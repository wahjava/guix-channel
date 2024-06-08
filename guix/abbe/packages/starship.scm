(define-module (abbe packages starship)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (guix gexp)
  #:use-module (nonguix build-system binary)
  #:use-module (guix download)
  #:use-module ((guix licenses) :prefix license:)
  #:use-module (ice-9 match))

(define fake-hash (make-string 52 #\0))

(define (starship-arch system)
  (match system
    ("aarch64-linux" "aarch64")
    ("x86_64-linux" "x86_64")
    ("i686-linux" "i686")))

(define (starship-url version system)
  (let ([system (starship-arch system)])
    (string-append "https://github.com/starship/starship/releases/download/v"
		   version "/starship-" system 
		   "-unknown-linux-musl.tar.gz")))

(define (starship-hash system)
  (match system
    ("aarch64-linux" "01gjxi4fih0z4693vszx715xa7khdkylqn1xy1dc8bzqrbz2spzn")
    ("x86_64-linux" "0nk7da0lyqfk6ikkc946vz6k6j9bi58si1z7119237nqsw336a54")
    ("i686-linux" "1pig20hyfpn58wfx7zky4mxxcq57zwlx9aywcg3y4vmy0s6gwy03")))

(define-public starship
  (package
   (name "starship")
   (version "1.19.0")
   (source (origin
            (method url-fetch/tarbomb)
            (uri (starship-url version (%current-system)))
            (sha256 (base32 (starship-hash (%current-system))))))
   (build-system binary-build-system)
   (arguments
    '(#:install-plan
     '(("starship" "bin/"))))
   (synopsis "The minimal, blazing-fast, and infinitely customizable prompt for any shell!")
   (description
    "The minimal, blazing-fast, and infinitely customizable prompt for any shell!")
   (home-page "https://starship.rs/")
   (license license:isc)))
