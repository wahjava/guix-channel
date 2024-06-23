(define-module (abbe packages symlinks)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build gnu-build-system)
  #:use-module (guix build utils)
  #:use-module (guix packages)
  #:use-module (guix licenses))

(define-public symlinks
  (package
   (name "symlinks")
   (version "1.4")
   (source
    (origin
     (method url-fetch)
     (uri (string-append "http://deb.debian.org/debian/pool/main/s/"
                         name "/" name "_" version ".orig.tar.gz"))
     (sha256
      (base32 "1683psyi8jwq6anhnkwwyaf7pfksf19v04fignd6vi52s2fnifxh"))))
    (build-system gnu-build-system)
    (arguments
      (list 
        #:phases
        '(modify-phases %standard-phases
                       (delete 'configure)
                       (delete 'check)
                       (replace 'install
                                (lambda* (#:key outputs #:allow-other-keys)
                                         (let* ((out (assoc-ref outputs "out"))
                                                (bin (string-append out "/bin"))
                                                (man (string-append out "/share/man/man8")))
                                           (install-file "symlinks" bin)
                                           (install-file "symlinks.8" man)
                                           ))))))
    (home-page "https://salsa.debian.org/debian/symlinks")
    (synopsis "scan/change symbolic links")
    (description "Symlinks scans directories for symbolic links and lists them on stdout. Each link is prefixed with a classification of relative, absolute, dangling, messy, lengthy or other_fs.")
    (license (fsf-free "https://bugs.debian.org/273338" "freely distributable"))))
