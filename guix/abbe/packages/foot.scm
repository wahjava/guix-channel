(define-module (abbe packages foot)
               #:use-module (guix packages)
               #:use-module (guix git-download)
               #:use-module (gnu packages terminals))

(define-public foot-1-18
               (package/inherit foot
                                (name "foot")
                                (version "1.18.0")
                                (source
                                  (origin (method git-fetch)
                                          (uri (git-reference
                                                 (url "https://codeberg.org/dnkl/foot")
                                                 (commit version)))
                                          (file-name (git-file-name name version))
                                          (sha256 (base32 "1znfk64kimm0vr3alvj66i1yn5glig3bw60n2lv9cn4jzi1b7qhb"))))))
