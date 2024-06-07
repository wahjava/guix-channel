(define-module (abbe packages weechat)
  #:use-module (guix packages)
  #:use-module (gnu packages irc)
  #:use-module (gnu packages javascript)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module ((guix licenses) :prefix license:))

(define-public weechat-4-3-2
  (package
    (inherit weechat)
    (name "weechat")
    (version "4.3.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://weechat.org/files/src/weechat-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "1iipzi67w1f13xllzhjhmnk9s4zn9fglxh7las8riq9yvpiivwz1"))))
    (inputs (modify-inputs (package-inputs weechat)
			   (append cjson)))))
