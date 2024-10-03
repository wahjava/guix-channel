(define-module (abbe build-system nix-rust)
  #:use-module (guix build-system)
  #:use-module (guix build utils)
  #:use-module (guix utils)
  #:use-module (guix monads)
  #:use-module (guix store)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix base32)
  #:use-module (guix search-paths)
  #:use-module (gnu packages certs)
  #:use-module (gnu packages version-control)
  #:export (nix-rust-build-system))

(define %rustup-metadata
  (quote (("aarch64-unknown-linux-gnu" (#:target . "aarch64-unknown-linux-gnu") (#:version . "1.81.0") (#:origins ((#:name . "cargo") (#:url . "https://static.rust-lang.org/dist/2024-09-05/cargo-1.81.0-aarch64-unknown-linux-gnu.tar.xz") (#:sha256 . "048vy4k8d5d65vghn3vklarl7630yclrmy8fnr8nrhi395z95y3n")) ((#:name . "rust-std") (#:url . "https://static.rust-lang.org/dist/2024-09-05/rust-std-1.81.0-aarch64-unknown-linux-gnu.tar.xz") (#:sha256 . "1x76xh31djdbajicp5m9b0b24390gxx2idprr278yczfgh1pyml5")) ((#:name . "rustc") (#:url . "https://static.rust-lang.org/dist/2024-09-05/rustc-1.81.0-aarch64-unknown-linux-gnu.tar.xz") (#:sha256 . "1z66s9ifxgpycp1d31h6d4xmr8jy38hypiqamnx2xigq70gna7rh")))) ("x86_64-unknown-linux-gnu" (#:target . "x86_64-unknown-linux-gnu") (#:version . "1.81.0") (#:origins ((#:name . "cargo") (#:url . "https://static.rust-lang.org/dist/2024-09-05/cargo-1.81.0-x86_64-unknown-linux-gnu.tar.xz") (#:sha256 . "0hdq71yb28711pbcp9ha1w77lz6vvmjm8vg360cld5c6msqy83n5")) ((#:name . "rust-std") (#:url . "https://static.rust-lang.org/dist/2024-09-05/rust-std-1.81.0-x86_64-unknown-linux-gnu.tar.xz") (#:sha256 . "0bsnnx3z1jdv0ydj7y9xhn2pib71d3yqkfh8cfaskvp8akr81pvd")) ((#:name . "rustc") (#:url . "https://static.rust-lang.org/dist/2024-09-05/rustc-1.81.0-x86_64-unknown-linux-gnu.tar.xz") (#:sha256 . "0gk1layk7h5ax2qzasc9jpb5iz2vbbg4mv2j1i54zgnfvr64x2lq")))))))

(define (default-mold-linker)
  (let ((mold (resolve-interface '(gnu packages mold))))
    (module-ref mold 'mold-as-ld-wrapper)))

(define (default-patchelf)
  (let ((elf (resolve-interface '(gnu packages elf))))
    (module-ref elf 'patchelf)))

(define (default-gcc)
  (let ((gcc (resolve-interface '(gnu packages gcc))))
    (module-ref gcc 'gcc)))

(define (default-libc)
  (let ((base (resolve-interface '(gnu packages base))))
    (module-ref base 'glibc)))

(define (default-zlib)
  (let ((mod (resolve-interface '(gnu packages compression))))
    (module-ref mod 'zlib)))

(define %nix-rust-build-system-modules
  `((guix build utils)
    (abbe build nix-rust-build-system)
    ,@%default-gnu-imported-modules))

(define (make-rust-toolchain target)

  (define %components '("cargo" "rustc" "rust-std"))

  (define entry (assoc-ref %rustup-metadata (nix-system->gnu-triplet target)))

  (define (origin->package-origin entry)
    (list (assoc-ref entry #:name)
          (origin (method url-fetch)
                  (uri (assoc-ref entry #:url))
                  (sha256 (base32 (assoc-ref entry #:sha256))))))
  (package
    (name "rust-toolchain")
    (version (assoc-ref entry #:version))
    (source #f)
    (synopsis "Rust toolchain")
    (native-inputs (cons
                    (list "patchelf" (default-patchelf))
                    (map origin->package-origin
                         (assoc-ref entry #:origins))))
    (description "Rust programming language toolchain")
    (home-page "https://rust-lang.org")
    (inputs (list (list "gcc" (default-gcc) "lib")
                  (list "zlib" (default-zlib))
                  (list "libc" (default-libc))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:modules `((ice-9 rdelim)
                  ,@%default-gnu-imported-modules)
      #:phases
      (with-imported-modules `((ice-9 rdelim)
                               ,@%default-gnu-imported-modules)
        #~(modify-phases %standard-phases
            (delete 'boostrap)
            (delete 'build)
            (delete 'check)
            (delete 'configure)
            (replace 'unpack
              (lambda* (#:key inputs #:allow-other-keys)
                (for-each (lambda (input)
                            (let ((base-dir
                                   (string-append input
                                                  "-"
                                                  #$(assoc-ref entry #:version)
                                                  "-"
                                                  #$(assoc-ref entry #:target)
                                                  "/"))
                                  (component-pathname (if (string=? input "rust-std")
                                                          (string-append input "-" #$(assoc-ref entry #:target))
                                                          input)))
                              (mkdir-p input)
                              (invoke "tar" "-xvf" (assoc-ref inputs input)
                                      "--transform"
                                      (string-append "s,^" base-dir ",,")
                                      "-C"
                                      input)))
                          (list #+@%components))))

            (replace 'install
              (lambda* (#:key outputs inputs #:allow-other-keys)
                (letrec* ((out (assoc-ref outputs "out"))
                          (libc (assoc-ref inputs "libc"))
                          (zlib (assoc-ref inputs "zlib"))
                          (gcc (assoc-ref inputs "gcc"))
                          (libs (map (lambda (d) (string-append d "/lib"))
                                     (list gcc libc zlib out)))
                          (rpath (string-join libs ":"))
                          ;; inspired from nonguix
                          (interpreter (car (find-files libc "ld-linux.*\\.so")))
                          (file-size (lambda (file)
                                       (stat:size (lstat file))))
                          (patch-elf
                           (lambda* (file rpath #:optional (interpreter #f))
                             (if (< (file-size file) 256)
                                 (let ((head-line (with-input-from-file file read-line)))
                                   (when (string-prefix? "INPUT(" head-line)
                                     (patch-elf
                                      (string-append (dirname file) "/"
                                                     (substring head-line (string-length "INPUT(")
                                                                (1- (string-length head-line))))
                                      rpath
                                      interpreter)))
                                 (if interpreter
                                     (invoke "patchelf" "--set-rpath" rpath "--set-interpreter" interpreter file)
                                     (invoke "patchelf" "--set-rpath" rpath file))))))

                  (mkdir-p out)
                  (for-each (lambda (input)
                              (with-directory-excursion input
                                (invoke "./install.sh"
                                        "--verbose"
                                        (string-append "--prefix=" out))))
                            (list #+@%components))

                  (chdir out)
                  (for-each (lambda (filename)
                              (display (string-join (list "patchelf" "--set-rpath" rpath filename) " "))
                              (newline)
                              (patch-elf filename rpath))
                            (find-files "lib" "\\.so$"))

                  (for-each (lambda (filename)
                              (display (string-join (list "patchelf" "--set-rpath" rpath "--set-interpreter" interpreter filename) " "))
                              (newline)
                              (patch-elf filename rpath interpreter))
                            (append
                             (find-files (string-append "lib/rustlib/"
                                                        #$(assoc-ref entry #:target)
                                                        "/bin"))
                             (find-files "libexec")
                             (map (lambda (f) (string-append "bin/" f))
                                  (list "rustc" "rustdoc" "cargo")))))))))))
    (license (list license:asl2.0 license:expat))))

(define rust-toolchain
  (make-rust-toolchain (%current-system)))

(define* (lower name
                #:key source inputs native-inputs outputs system target
                (build-flags '())
                (env-vars '())
                (vendor-hash #f)
                #:allow-other-keys
                #:rest arguments)
  "Return a bag for NAME."

  (define private-keywords
    '(#:target #:inputs #:native-inputs #:vendor-hash))

  (define* (cargo-vendor-fetch source hash-algo hash name #:key system)
    (gexp->derivation (string-append name "-vendor")
      #~(begin
          (let ((tmpdir (or (getenv "TMPDIR") "/tmp")))
            (use-modules (guix build utils)
                         (guix search-paths))

            (setenv "PATH" (cdar (evaluate-search-paths (list $PATH)
                                                        (list
                                                         #$git-minimal #$rust-toolchain
                                                         #$@(map cadr (standard-packages))))))

            (setenv "SSL_CERT_DIR" (string-append #$nss-certs "/etc/ssl/certs"))

            (chdir #$source)
            (mkdir-p (string-append #$output "/.cargo"))
            (setenv "CARGO_HOME"
                    (string-append tmpdir "/.cargo"))

            (mkdir-p #$output)

            (invoke "cargo" "vendor" "-q" "--locked" "--versioned-dirs"
                    #$output)))

      #:modules
      '((guix build utils)
        (guix records)
        (guix search-paths))
      #:local-build? #t
      #:recursive? #t
      #:system system
      #:hash-algo hash-algo
      #:hash hash
      #:leaked-env-vars '("http_proxy" "https_proxy" "LC_ALL" "LC_MESSAGES" "LANG" "COLUMNS")))

  (define (vendor-origin name vendor-hash)
    (origin (method cargo-vendor-fetch)
            (uri source)
            (file-name name)
            (hash (content-hash (nix-base32-string->bytevector vendor-hash) sha256))))

  (bag
    (name name)
    (system system)
    (target target)
    (build-inputs `(,@(if source
                          `(("source" ,source))
                          '())
                    ,@(if vendor-hash
                          `(("vendor" ,(vendor-origin name vendor-hash)))
                          '())
                    ("rust-toolchain" ,rust-toolchain)
                    ("mold" ,(default-mold-linker))
                    ,@native-inputs
                    ,@inputs
                    ,@(if target
                          ;; Use the standard cross inputs of
                          ;; 'gnu-build-system'.
                          (standard-cross-packages target 'host)
                          '())
                    ;; Keep the standard inputs of 'gnu-build-system'.
                    ,@(standard-packages)))
    (host-inputs '())

    ;; The cross-libc is really a target package, but for bootstrapping
    ;; reasons, we can't put it in 'host-inputs'.  Namely, 'cross-gcc' is a
    ;; native package, so it would end up using a "native" variant of
    ;; 'cross-libc' (built with 'gnu-build'), whereas all the other packages
    ;; would use a target variant (built with 'gnu-cross-build'.)
    (target-inputs (if target
                       (standard-cross-packages target 'target)
                       '()))

    (outputs outputs)
    (build nix-rust-build)
    (arguments (cons* #:build-flags build-flags
                      (strip-keyword-arguments private-keywords arguments)))))

(define* (nix-rust-build name inputs
                       #:key
                       source
                       (env-vars '(list))
                       (phases '%standard-phases)
                       (outputs '("out"))
                       (search-paths '())
                       (install-source? #f)
                       (build-flags '())
                       (tests? #t)
                       (parallel-build? #t)
                       (parallel-tests? #t)
                       (system (%current-system))
                       (guile #f)
                       (imported-modules %nix-rust-build-system-modules)
                       (modules '((abbe build nix-rust-build-system)
                                  (guix build utils)))
                       (substitutable? #t))

  (define builder
    (with-imported-modules imported-modules
      #~(begin
          (use-modules #$@(sexp->gexp modules))
          (nix-rust-build #:name #$name
                          #:source #+source
                          #:system #$system
                          #:phases #$phases
                          #:outputs #$(outputs->gexp outputs)
                          #:substitutable? #$substitutable?
                          #:search-paths '#$(sexp->gexp
                                           (map search-path-specification->sexp
                                                search-paths))
                          #:install-source? #$install-source?
                          #:build-flags (list #$@build-flags)
                          #:tests? #$tests?
                          #:parallel-build? #$parallel-build?
                          #:parallel-tests? #$parallel-tests?
                          #:rust-target #$(nix-system->gnu-triplet (%current-system))
                          #:cc #$(cc-for-target)
                          #:env-vars #$env-vars
                          #:inputs #$(input-tuples->gexp inputs)))))

  (mlet %store-monad ((guile (package->derivation (or guile (default-guile))
                                                  system #:graft? #f)))
    (gexp->derivation name builder
                      #:system system
                      #:guile-for-build guile)))

(define nix-rust-build-system
  (build-system
    (name 'nix-rust)
    (description
     "Build system for Rust programs")
    (lower lower)))
