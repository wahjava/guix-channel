(define-module (abbe packages ssh)
  #:use-module (guix packages)
  #:use-module (gnu packages ssh)
  #:use-module (guix download)
  #:use-module ((guix licenses) :prefix license:))

(define-public openssh-99
  (let ((version "9.9p1"))
    (package
      (inherit openssh)
      (name "openssh")
      (version version)
      (source (origin
		            (method url-fetch)
                (uri (string-append "mirror://openbsd/OpenSSH/portable/"
                          "openssh-" version ".tar.gz"))
		            (sha256
                 (base32 "00kcjs7vm1vha3xvgrkb0qv7v6pwskb1avkfk2qiazzqpz6znhxk"))
                (patches (origin-patches (package-source openssh))))))))

(define-public openssh-sans-x-99
  (let ((version "9.9p1"))
    (package
      (inherit openssh-sans-x)
      (name (package-name openssh-sans-x))
      (version version)
      (source (origin
		            (method url-fetch)
                (uri (string-append "mirror://openbsd/OpenSSH/portable/"
                          "openssh-" version ".tar.gz"))
		            (sha256
                 (base32 "00kcjs7vm1vha3xvgrkb0qv7v6pwskb1avkfk2qiazzqpz6znhxk"))
                (patches (origin-patches (package-source openssh-sans-x))))))))
