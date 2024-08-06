(define-module (abbe packages zfs)
    #:use-module (guix packages)
    #:use-module (guix download)
    #:use-module (gnu packages file-systems))

(define-public zfs-2-2-5
    (package/inherit zfs
        (version "2.2.5")
        (source
          (origin (method url-fetch)
                  (uri (string-append "https://github.com/openzfs/zfs/releases"
                                      "/download/zfs-" version
                                      "/zfs-" version ".tar.gz"))
                  (sha256
                   (base32 "15w8s0f155kpylgdan56hgwc86fl1658br05dmyyhxfd55pwz213"))))))
                     
