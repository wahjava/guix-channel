(define-module (abbe packages dns)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix base16)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages dns))

(define-public knot-3.3.8
  (package/inherit knot
    (version "3.3.8")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.nic.cz/knot/knot-dns")
             (commit (string-append "v" version))))
       (file-name (git-file-name (package-name knot) version))
       (sha256
        (base32 "0iaardlmvcp6f0vccs81f202bb53y7fkcw5n12ahgqymqzhafpmq"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; Remove Ragel-generated C files.  We'll recreate them below.
           (for-each delete-file (find-files "." "\\.c\\.[gt]."))
           (delete-file "src/libknot/yparser/ypbody.c")
           ;; Remove bundled libraries to ensure we always use the system's.
           (with-directory-excursion "src/contrib"
             (for-each delete-file-recursively
                       ;; TODO: package libngtcp2 for DoQ (‘QUIC’) support.
                       '("libngtcp2")))))))
    (inputs (modify-inputs (package-inputs knot)
              (append ngtcp2)))))

(define-public knot-resolver-5.7.4
  (package/inherit knot-resolver
    (version "5.7.4")
    (inputs (modify-inputs (package-inputs knot-resolver)
              (replace "knot" (list knot-3.3.8 "lib"))))
    (source (origin
              (method url-fetch)
              (uri (string-append "https://secure.nic.cz/files/knot-resolver/"
                                  "knot-resolver-" version ".tar.xz"))
              (sha256
               (base32
                "1j6rig8mb4sh11q6cfhqmlsaxw41fwiglkflz8d08a38y3nacvbb"))))))
