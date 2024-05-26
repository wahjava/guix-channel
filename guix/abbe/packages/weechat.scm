(define-module (abbe packages weechat)
  #:use-module (guix packages)
  #:use-module (gnu packages irc)
  #:use-module (gnu packages javascript)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module ((guix licenses) :prefix license:))

(define-public weechat-4-3-0
  (package
    (inherit weechat)
    (name "weechat")
    (version "4.3.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://weechat.org/files/src/weechat-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "0bc6jvvsprrpnr0bjfi6z0h0z3x0dkq5h38y9nipg7zny85zky9n"))))
    (inputs (modify-inputs (package-inputs weechat)
			   (append cjson)))))
