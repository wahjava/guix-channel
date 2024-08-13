(define-module (abbe packages clasp)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix search-paths)
  #:use-module (guix utils)
  #:use-module (gnu packages base)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages libunwind)
  #:use-module (gnu packages lisp)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages ninja)
  #:use-module (gnu packages pretty-print)
  #:use-module (gnu packages pkg-config)
  #:use-module ((guix licenses) #:prefix license:))

(define-public clasp-cl
  (package
    (name "clasp-cl")
    (version "2.6.0")
    (source (origin (method url-fetch)
                    (uri (string-append "https://github.com/clasp-developers/clasp/releases/download/" version
                                        "/clasp-" version ".tar.gz"))
                    (sha256 (base32 "10jjhcid6qp64gx29iyy5rqqijwy8hrvx66f0xabdj8w3007ky39"))))
    (native-inputs (list sbcl
                         ninja
                         pkg-config
                         binutils-gold))
    (inputs (list boost fmt gmp libelf libunwind clang-15 llvm-15
                  (list gcc "lib")))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'check)
         (add-after 'unpack 'patch-koga
           (lambda* _
             (call-with-port (open-file "src/koga/units.lisp" "a")
               (lambda (p)
                 (display "(defmethod configure-unit (c (u (eql :git))))\n" p)))))
         (add-before 'configure 'set-configure-environment
           (lambda* _
             (setenv "SOURCE_DATE_EPOCH" "1")
             (setenv "ASDF_OUTPUT_TRANSLATIONS"
                     (string-append (getenv "PWD")
                                    ":"
                                    (getenv "PWD")
                                    "/__fasls"))))
         (replace 'configure
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (clang (assoc-ref inputs "clang"))
                   (ld-flags (string-join (apply append (map (lambda (f)
                                                               (list "-L" f "-rpath" f))
                                                             (string-split (getenv "LIBRARY_PATH") #\:)))
                                          ",")))
               (invoke "sbcl"
                       "--script"
                       "./koga"
                       "--skip-sync"
                       "--build-mode=bytecode-faso"
                       (string-append "--cc=" clang "/bin/clang")
                       (string-append "--cxx=" clang "/bin/clang++")
                       (string-append "--ldflags=-Wl," ld-flags)
                       "--reproducible-build"
                       "--package-path=/"
                       (string-append "--bin-path=" out "/bin")
                       (string-append "--lib-path=" out "/lib")
                       (string-append "--share-path=" out "/share")))))
         (replace 'build
           (lambda* _
             (invoke "ninja" "-C" "build")))
         (replace 'install
           (lambda* _
             (invoke "ninja" "-C" "build" "install"))))))
    (home-page "https://clasp-developers.github.io/")
    (synopsis "Clasp is a Common Lisp implementation.")
    (description "Clasp is a new Common Lisp implementation that seamlessly
 interoperates with C++ libraries and programs using LLVM for compilation to
 native code. This allows Clasp to take advantage of a vast array of
 preexisting libraries and programs, such as out of the scientific computing
 ecosystem. Embedding them in a Common Lisp environment allows you to make use
 of rapid prototyping, incremental development, and other capabilities that
 make it a powerful language.")
    (license license:lgpl2.1)))
