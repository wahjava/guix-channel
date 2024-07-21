(define-module (abbe packages ghc)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system copy)
  #:use-module (guix base16)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages ncurses)
  #:use-module (ice-9 match)
  #:use-module ((guix licenses) :prefix license:))

(define (ghc-bindist-url)
  (match (%current-system)
    ("aarch64-linux" "https://downloads.haskell.org/~ghc/9.6.6/ghc-9.6.6-aarch64-deb10-linux.tar.xz")
    ("x86_64-linux" "https://downloads.haskell.org/~ghc/9.6.6/ghc-9.6.6-x86_64-deb11-linux.tar.xz")))

(define (stack-bindist-url)
  (match (%current-system)
    ("x86_64-linux" "https://github.com/commercialhaskell/stack/releases/download/v2.15.7/stack-2.15.7-linux-x86_64.tar.gz")
    ("aarch64-linux" "https://github.com/commercialhaskell/stack/releases/download/v2.15.7/stack-2.15.7-linux-aarch64.tar.gz")))

(define (ghc-bindist-sha256)
  (match (%current-system)
    ("aarch64-linux" "58d5ce65758ec5179b448e4e1a2f835924b4ada96cf56af80d011bed87d91fef")
    ("x86_64-linux" "a34bdfc1f65b000135d9c8eb12d69670026a64043a8b33ef5ba24b0f8e28d046")))

(define (stack-bindist-sha256)
  (match (%current-system)
    ("aarch64-linux" "f0c4b038c7e895902e133a2f4c4c217e03c4be44aa5da48aec9f7947f4af090b")
    ("x86_64-linux" "4e635d6168f7578a5694a0d473c980c3c7ed35d971acae969de1fd48ef14e030")))

(define (hls-bindist-url)
  (match (%current-system)
    ("x86_64-linux" "https://downloads.haskell.org/~hls/haskell-language-server-2.9.0.1/haskell-language-server-2.9.0.1-x86_64-linux-unknown.tar.xz")
    ("aarch64-linux" "https://downloads.haskell.org/~hls/haskell-language-server-2.9.0.1/haskell-language-server-2.9.0.1-aarch64-linux-ubuntu20.tar.xz")))

(define (hls-bindist-sha256)
  (match (%current-system)
    ("x86_64-linux" "025xhk2vp4z0ad94yzbbv80m5njp71sxwpbwickwzmjlc95yyb5n")
    ("aarch64-linux" "1d0nxzs9lil1fjcxkhdfki474xzv2f5z4y539c8i35a97lrddjs2")))

(define-public ghc
  (package
    (name "ghc")
    (version "9.6.6")
    (source
     (origin (method url-fetch)
             (uri (ghc-bindist-url))
             (sha256 (base16-string->bytevector (ghc-bindist-sha256)))))
    (build-system gnu-build-system)
    (inputs (list gmp glibc ncurses/tinfo))
    (native-inputs (list binutils-gold gcc-toolchain llvm gnu-make patchelf))
    (arguments
     `(#:configure-flags
       (list ,(string-append "--target="
                             (or (%current-target-system)
                                 (nix-system->gnu-triplet
                                  (%current-system)))))
       #:strip-binaries? #f
       #:phases
       ,#~(modify-phases %standard-phases
            (add-before 'configure 'patchelf-binaries
              (lambda* (#:key version system inputs outputs #:allow-other-keys)
                (let* ((binaries (map (lambda (f) (string-append "bin/" f))
                                      '("unlit" "haddock" "ghc-iserv-prof"
                                        "ghc-iserv" "ghc-iserv-dyn" "ghc-pkg"
                                        "hp2ps" "hpc" "ghc" "hsc2hs" "runghc"
                                        "runhaskell")))
                       (gmp (assoc-ref inputs "gmp"))
                       (gmp-lib (string-append gmp "/lib"))
                       (ncurses (assoc-ref inputs "ncurses-with-tinfo"))
                       (ncurses-lib (string-append ncurses "/lib"))
                       (out (assoc-ref outputs "out"))
                       (libdir (string-append "lib/" system "-ghc-9.6.6"))
                       (out-lib (string-append out "/lib/ghc-9.6.6/" libdir ))
                       (ld-so (search-input-file inputs #$(glibc-dynamic-linker)))
                       (rpath (string-join (list gmp-lib out-lib ncurses-lib
                                                 (dirname ld-so)) ":"))
                       (libs (find-files libdir "libHS.+\\.so")))
                  (for-each (lambda (file)
                              (invoke "patchelf" "--set-interpreter" ld-so
                                      "--set-rpath" rpath
                                      file))
                            (append binaries '
                                    ("lib/bin/ghc-iserv"
                                     "lib/bin/ghc-iserv-prof"
                                     "lib/bin/ghc-iserv-dyn")))
                  (for-each (lambda (file)
                              (invoke "patchelf" "--set-rpath" rpath file))
                            libs))))
            (delete 'build)
            (delete 'check))))
    (synopsis "Glassgow Haskell Compiler")
    (description "The Glasgow Haskell Compiler (GHC) is a state-of-the-art compiler and interactive environment for the functional language Haskell.")
    (home-page "https://www.haskell.org/ghc")
    (license license:bsd-3)))

(define-public cabal-install
  (package
    (name "cabal-install")
    (version "3.10.1.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                     "https://downloads.haskell.org/~cabal/cabal-install-"
                     version
                     "/cabal-install-"
                     version
                     "-"
                     (%current-system)
                     "-alpine.tar.xz"))
             (sha256 (base32 "16fx3hg5fz2lx5adz25b0jkqhilf3jd4c7hjx9x69n4vwvba6zqq"))))
    (build-system copy-build-system)
    (arguments
      (list #:install-plan
	    ''(("cabal" "bin/"))))
    (synopsis "cabal-install tool")
    (description "cabal-install tool")
    (home-page "https://www.haskell.org/cabal/")
    (license license:bsd-3)))

(define-public stack
  (package
    (name "stack")
    (version "2.15.7")
    (source (origin
              (method url-fetch)
              (uri (stack-bindist-url))
              (sha256 (base16-string->bytevector (stack-bindist-sha256)))))
    (build-system copy-build-system)
    (arguments
      (list #:install-plan
	    ''(("stack" "bin/")
	       ("doc" "share/doc/stack"))))
    (synopsis "stack tool")
    (description "stack tool")
    (home-page "https://haskellstack.org/")
    (license license:bsd-3)))

(define-public haskell-language-server
  (package
    (name "haskell-language-server")
    (version "2.9.0.1")
    (source (origin
              (method url-fetch)
              (uri (hls-bindist-url))
              (sha256 (base32 (hls-bindist-sha256)))))
    (build-system gnu-build-system)
    (inputs (list gmp glibc zlib))
    (native-inputs (list patchelf))
    (arguments
     (list
      #:strip-binaries? #f
      #:validate-runpath? #f
      #:make-flags
      '(list (string-append "PREFIX=" %output))
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (delete 'build)
          (delete 'check)
          (add-before 'install 'patchelf-binaries
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let* ((rpath (string-join (map (lambda (input)
                                                (string-append (assoc-ref inputs input) "/lib"))
                                              '("glibc" "gmp" "zlib"))
                                         ":"))
                     (ld-so (search-input-file inputs #$(glibc-dynamic-linker)))
                     (out (assoc-ref outputs "out"))
                     (hls-prefix-length (string-length "bin/haskell-language-server-")))
                (for-each (lambda (file)
                            (let ((hls-ver (substring file hls-prefix-length)))
                              (invoke "patchelf"
                                      "--set-rpath"
                                      (if (string-ci=? "wrapper" hls-ver)
                                          rpath
                                          (string-append rpath ":"
                                                         out "/lib/haskell-language-server-2.9.0.1/lib/"
                                                         hls-ver))
                                      "--set-interpreter"
                                      ld-so
                                      file)))
                          (find-files "bin" "^haskell-language-server-.*$"))))))))
    (synopsis "Haskell Language Server Implementation")
    (description "Haskell LSP")
    (home-page "https://github.com/haskell/haskell-language-server")
    (license license:asl2.0)))
