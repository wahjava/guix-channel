(define-module (abbe packages mold)
  #:use-module (guix packages)
  #:use-module (gnu packages mold)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module ((guix licenses) :prefix license:))

(define-public mold-2-32
  (package
    (inherit mold)
    (name "mold")
    (version "2.33.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rui314/mold")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0wsb0aiqia3jfc9k3h2d446y0mzmaq6rz9xkjf9125npdygm7kpb"))
       (modules '((guix build utils)))
       (snippet
        #~(begin
            (for-each
             (lambda (x)
               (delete-file-recursively (string-append "third-party/" x)))
             '("mimalloc" "tbb" "xxhash" "zlib" "zstd"))))))))
