(define-module (abbe packages weechat)
  #:use-module (guix packages)
  #:use-module (gnu packages irc)
  #:use-module (gnu packages javascript)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module ((guix licenses) :prefix license:))

(define-public weechat-4-3-1
  (package
    (inherit weechat)
    (name "weechat")
    (version "4.3.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://weechat.org/files/src/weechat-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "0lw6s5864nrdxp9bxzh5x1s68y0d8w21lqrraxk3qc6cgnhj4zhm"))))
    (inputs (modify-inputs (package-inputs weechat)
			   (append cjson)))))
