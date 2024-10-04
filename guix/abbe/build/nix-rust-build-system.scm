(define-module (abbe build nix-rust-build-system)
  #:use-module (guix build utils)
  ;; #:use-module (guix utils)
  #:use-module ((guix build gnu-build-system) #:prefix gnu:)
  #:export (%standard-phases
            nix-rust-build))

(define* (configure #:key name  inputs rust-target cc env-vars parallel-build? #:allow-other-keys)
  (let* ((vendor (assoc-ref inputs "vendor"))
         (source (assoc-ref inputs "source"))
         (tmpdir (getenv "TMPDIR"))
         (vendor-dest (string-append tmpdir "/cargo-vendor-" name)))

    ;; use a symlink-ed copy otherwise it embeds references to /gnu/store/...-vendor
    (symlink vendor vendor-dest)

    (mkdir-p ".cargo")
    (call-with-output-file ".cargo/config.toml"
      (lambda (cc-toml)
        (display "[source.cargo]\n" cc-toml)
        (format cc-toml "directory = ~s\n" vendor-dest)
        (display "[source.crates-io]\n" cc-toml)
        (display "replace-with = 'cargo'\n" cc-toml)
        (format cc-toml "[target.~a]\n" rust-target)
        (format cc-toml "linker = ~s\n" cc)))

    (setenv "CC" cc)
    (setenv "CARGO_HOME" (string-append (getenv "TMPDIR") "/.cargo"))
    (setenv "CARGO_VENDOR_DIR" vendor-dest)
    (setenv "CARGO_MANIFEST_DIR" source)
    (setenv "CARGO_BUILD_TARGET" rust-target)
    (setenv "RUST_BACKTRACE" "full")

    (when parallel-build?
      (setenv "CARGO_BUILD_JOBS" (number->string (parallel-job-count))))

    (invoke "cargo" "update" "--manifest-path" "Cargo.toml" "--verbose")

    (for-each (lambda (env) (setenv (car env) (cdr env)))
              env-vars)))

(define* (build #:rest _)
  (invoke "cargo" "build" "--release"))

(define* (install #:key outputs cargo-install-paths #:allow-other-keys)
  (let ((out (assoc-ref outputs "out")))
    (mkdir-p out)
    (for-each (lambda (path)
                (invoke "cargo" "install"
                        "--path" path
                        "--root" out
                        "--verbose"))
              cargo-install-paths)))

(define* (delete-crate-metadata #:key outputs #:allow-other-keys)
  (let ((out (assoc-ref outputs "out")))

    (for-each delete-file (list (string-append out "/.crates.toml")
                                (string-append out "/.crates2.json")))))

(define %standard-phases
  (modify-phases gnu:%standard-phases
    (delete 'bootstrap)
    (delete 'check)
    (replace 'configure configure)
    (replace 'build build)
    (replace 'install install)
    (add-after 'install 'delete-crate-metadata delete-crate-metadata)))

(define* (nix-rust-build #:key inputs (phases %standard-phases) #:allow-other-keys #:rest args)
  (apply gnu:gnu-build #:inputs inputs #:phases phases args))
