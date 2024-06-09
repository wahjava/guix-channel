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
    (version "2.32.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rui314/mold")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1wl7mp7r5hxmvfpmrq32ffjpgn8z8pk775y423nr56gvrb39vj6i"))
       (modules '((guix build utils)))
       (snippet
        #~(begin
            (for-each
             (lambda (x)
               (delete-file-recursively (string-append "third-party/" x)))
             '("mimalloc" "tbb" "xxhash" "zlib" "zstd"))))))))
