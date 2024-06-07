(define-module (abbe packages pinentry)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages gnupg))

(define-public pinentry-130
  (package
   (inherit pinentry-tty)
   (name "pinentry-tty")
   (version "1.3.0")
   (source
    (origin
     (method url-fetch)
              (uri (string-append "mirror://gnupg/pinentry/pinentry-"
                                  version ".tar.bz2"))
     (sha256
      (base32 "1pllchqwl6wq4q4dpsarcm9m2cwjb733p6irxpyz55vmdqidag4v"))))))

(define-public pinentry-qt-130
  (package
   (inherit pinentry-qt)
   (name "pinentry-qt")
   (version "1.3.0")
   (source
    (origin
     (method url-fetch)
              (uri (string-append "mirror://gnupg/pinentry/pinentry-"
                                  version ".tar.bz2"))
     (sha256
      (base32 "1pllchqwl6wq4q4dpsarcm9m2cwjb733p6irxpyz55vmdqidag4v"))))
   (inputs
     (modify-inputs (package-inputs pinentry-qt)
		    (delete qtbase-5)
		    (prepend qtbase)))))
