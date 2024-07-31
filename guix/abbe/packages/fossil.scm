(define-module (abbe packages fossil)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages base)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages compression)
  #:use-module (nonguix build-system binary)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (guix modules)
  #:use-module ((guix licenses) :prefix license:))

(define current-module-file
  (search-path %load-path
                   (module-name->file-name
                    (module-name (current-module)))))

(define current-module-directory
  (dirname (and=> current-module-file canonicalize-path)))

(define-public fossil-2-24
  (define (fossil-patch name from to hash)
    (origin (method url-fetch)
      (uri (string-append "https://fossil-scm.org/home/vpatch?from=" from
            "&to=" to))
      (sha256 (base32 hash))
      (file-name name)))
  (package
    (inherit fossil)
    (name "fossil")
    (version "2.24")
    (source (origin
             (method url-fetch)
             (uri
              (string-append
               "https://fossil-scm.org/home/tarball/version-" version "/fossil-" version ".tar.gz"))
             (sha256
              (base32 "0k1gjvxbvvs98yspnf7nj6rb0c7yf60savq7jywbkgimd0bkrkcm"))
             (patch-flags '("-p0"))
             (patches (map (lambda (f) (local-file (string-append current-module-directory
                                                                  "/patches/" f)))
                           (list "fossil-disable-tests.patch"
                                 "fossil-fix-json-test.patch"
                                 "fossil-comment-utf-tests.patch")))))

    (arguments
      (substitute-keyword-arguments
        (package-arguments fossil)
        ((#:configure-flags _)
         #~(list "--with-openssl=auto"
                 "--enable-json"))))

    (inputs (modify-inputs (package-inputs fossil)
             (delete "sqlite")))))

(define-public fossil-bin
  (package/inherit fossil
    (name "fossil-bin")
    (version "2.24")
    (source (origin
             (method url-fetch/tarbomb)
             (uri
              (string-append
                "https://fossil-scm.org/home/uv/fossil-linux-x64-" version ".tar.gz"))
             (sha256 (base32 "0gxsbc9bnbjqdnip162x3di3s4k6dcvd8j8yyzg3jgw7z56hw5sm"))))
    (build-system binary-build-system)
    (inputs (list glibc zlib))
   (arguments
    '(#:install-plan
      '(("fossil" "bin/fossil"))
      #:strip-binaries? #f
      #:patchelf-plan
      '(("fossil")
        ("fossil" ("glibc" "zlib")))))))
