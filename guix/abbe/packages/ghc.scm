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

(define (ghc-bindist-sha256)
  (match (%current-system)
    ("aarch64-linux" "58d5ce65758ec5179b448e4e1a2f835924b4ada96cf56af80d011bed87d91fef")
    ("x86_64-linux" "a34bdfc1f65b000135d9c8eb12d69670026a64043a8b33ef5ba24b0f8e28d046")))

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
