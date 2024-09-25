(define-module (abbe packages iamb)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (gnu packages base)
  #:use-module (gnu packages gcc)
  #:use-module (nonguix build-system binary)
  #:use-module (guix download)
  #:use-module (guix licenses)
  #:use-module (ice-9 match))

(define (iamb-url version system)
  (match system
    ("aarch64-linux" (string-append "https://github.com/ulyssa/iamb/releases/download/v" version
                                    "/iamb-aarch64-unknown-linux-gnu.tgz"))
    ("x86_64-linux" (string-append "https://github.com/ulyssa/iamb/releases/download/v" version
                                    "/iamb-x86_64-unknown-linux-musl.tgz"))))
(define (iamb-hash version system)
  (match system
    ("aarch64-linux" "0h0s4ln16msanq9nvqggwyn45bxzf9xbjhyy2md174x93i8n2gql")
    ("x86_64-linux" "1hwwvi2j4mjxvdk4430l19vw81r3jw5g5fhqcxf0qmgav5bsr2nz")))

(define-public iamb
  (package
    (name "iamb")
    (version "0.0.10")
    (source (origin
              (method url-fetch)
              (uri (iamb-url version (%current-system)))
	            (sha256 (base32 (iamb-hash version (%current-system))))))
    (build-system binary-build-system)
    (arguments
     (list
      #:install-plan
      #~`(("iamb" "bin/iamb")
         ("docs/iamb.5" "share/man/man5/iamb.5")
         ("docs/iamb.1" "share/man/man1/iamb.1"))
      #:patchelf-plan
	    `'(("iamb" ("glibc" "gcc")))))
    (inputs `(,glibc (,gcc "lib")))
    (synopsis "A terminal-based client for Matrix for the Vim addict.")
    (description "iamb is a terminal-based client for Matrix for the Vim addict. You can edit messages, navigate windows and manage tabs in the same ways that your fingers are used to from your favorite text editor!")
    (home-page "https://iamb.chat/")
    (license asl2.0)))
