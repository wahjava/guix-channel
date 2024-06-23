(define-module (abbe packages weechat)
  #:use-module (guix packages)
  #:use-module (gnu packages irc)
  #:use-module (gnu packages javascript)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module ((guix licenses) :prefix license:))

(define-public weechat-4-3-3
  (package
    (inherit weechat)
    (name "weechat")
    (version "4.3.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://weechat.org/files/src/weechat-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "1n2y20mv7nj4n71qmzvnr87qxh4zkv8acihw23d5p29kx9ndp1sm"))))
    (inputs (modify-inputs (package-inputs weechat)
			   (append cjson)))))
