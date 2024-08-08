(define-module (abbe packages awscli)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (ice-9 match)
  #:use-module (nonguix build-system binary)
  #:use-module (gnu packages compression))

(define (awscli-arch)
  (match (%current-system)
    ("aarch64-linux" "linux-aarch64")
    ("x86_64-linux" "linux-x86_64")))

(define (awscli-hash)
  (match (%current-system)
    ("aarch64-linux" "1p73kqwakbgg8z1bnvv6zllqxl7rplry6qhdqh5k4cmm7z344lh7")
    ("x86_64-linux" "1rqfx1vlqcxm8csifq8nqy5c0czpjpafk9jx3hiicjpz29xrqysv")))

(define-public awscli
  (package
    (name "awscli")
    (version "2.17.25")
    (source (origin (method url-fetch)
                    (uri (string-append "https://awscli.amazonaws.com/awscli-exe-"
                                        (awscli-arch) "-" version ".zip"))
                    (sha256 (base32 (awscli-hash)))))
    (native-inputs (list unzip))
    (inputs (list zlib))
    (build-system binary-build-system)
    (arguments (list
                #:patchelf-plan
                ''(("dist/aws" ("zlib"))
                   ("dist/aws_completer" ("zlib")))
                #:phases
                #~(modify-phases %standard-phases
                    (delete 'configure)
                    (delete 'build)
                    (delete 'check)
                    (replace 'install
                      (lambda* (#:key outputs #:allow-other-keys)
                        (let* ((output (assoc-ref outputs "out"))
                               (bin-dir (string-append output "/bin")))
                          (mkdir-p bin-dir)
                          (invoke "./install" "-i" output "-b" bin-dir)))))))
    (home-page "https://aws.amazon.com/cli/")
    (synopsis "Universal Command Line Interface for Amazon Web Services")
    (description "This package provides a unified command line interface to Amazon Web Services.")
    (license license:asl2.0)))
