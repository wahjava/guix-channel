(define-module (abbe packages weechat)
  #:use-module (guix packages)
  #:use-module (gnu packages irc)
  #:use-module (gnu packages javascript)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module ((guix licenses) :prefix license:))

(define-public weechat-4-3-5
  (package
    (inherit weechat)
    (name "weechat")
    (version "4.3.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://weechat.org/files/src/weechat-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "1qsbdg3c0787xs0vwbxsyylf5fvz4cazrzlnwj2mnp6s6b4c9nz6"))))
    (inputs (modify-inputs (package-inputs weechat)
			   (append cjson)))))
