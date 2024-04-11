(define-module (abbe packages tmux)
  #:use-module (guix packages)
  #:use-module (gnu packages tmux)
  #:use-module (gnu packages bison)
  #:use-module (guix download))

(define-public tmux-34
  (package
    (inherit tmux)
    (name "tmux-34")
    (version "3.4")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/tmux/tmux/releases/download/"
                    version "/tmux-" version ".tar.gz"))
              (sha256
               (base32
		"1ahr7si3akr55hadyms3p36f1pbwavpkbfxpsq55ql5zl3gbh6jm"))))
    (native-inputs
     (modify-inputs (package-native-inputs tmux)
       (prepend bison)))))
