(define-module (abbe packages impala)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages base)
  #:use-module (guix gexp)
  #:use-module (guix build utils)
  #:use-module (guix utils)
  #:use-module (nonguix build binary-build-system)
  #:use-module (nonguix build-system binary)
  #:use-module (guix download)
  #:use-module (ice-9 match)
  #:use-module (guix licenses))

(define impala-version "0.2.1")

(define (impala-hash)
  (match (%current-system)
    ("aarch64-linux" "1fklimfaryqxab53dhxpylq02820vax5n29a0qpxap8sci2047dy")
    ("x86_64-linux" "04yp4b8vafzh3gs0hwrsc9snc2w0f7v15mmnzabyp6nnmmdwh1rn")))

(define (impala-filename)
  (match (%current-system)
    ("aarch64-linux" "impala-aarch64-unknown-linux-gnu")
    ("x86_64-linux"  "impala-x86_64-unknown-linux-gnu")))

(define-public impala
  (package
   (name "impala")
   (version impala-version)
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://github.com/pythops/impala/releases/download/v"
                  version "/" (impala-filename)))
            (sha256 (base32 (impala-hash)))))
   (build-system binary-build-system)
   (arguments
    `(#:patchelf-plan
      '(("sources/impala" ("glibc" "gcc")))
      #:install-plan
      ,#~(list
          (list "sources/impala" "bin/impala"))
      #:phases
      (modify-phases %standard-phases
                     (delete 'binary-unpack)
                     (replace 'unpack
                              (lambda* (#:key name source version #:allow-other-keys)
                                       (let* ((base-dir "sources")
                                              (exe (string-append base-dir "/impala")))
                                         (mkdir-p base-dir)
                                         (copy-file source exe)
                                         (chmod exe #o755)))))))
   (inputs
    (list
     glibc
     `(,gcc "lib")))
   (synopsis "TUI for managing wifi on Linux.")
   (description "TUI for managing wifi on Linux.")
   (home-page "https://github.com/pythops/impala")
   (license gpl3)))
