(define-module (abbe packages mold)
  #:use-module (guix packages)
  #:use-module (gnu packages mold)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module ((guix licenses) :prefix license:))

(define-public mold-2-31
  (package
    (inherit mold)
    (name "mold")
    (version "2.31.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rui314/mold")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1fs277z5wbjlz31nvbzihv8p84alk8iap50wpf1ffcq2h3aj8hh9"))
       (modules '((guix build utils)))
       (snippet
        #~(begin
            (for-each
             (lambda (x)
               (delete-file-recursively (string-append "third-party/" x)))
             '("mimalloc" "tbb" "xxhash" "zlib" "zstd"))))))))
