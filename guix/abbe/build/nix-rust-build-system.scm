(define-module (abbe build nix-rust-build-system)
  #:use-module (guix build utils)
  ;; #:use-module (guix utils)
  #:use-module ((guix build gnu-build-system) #:prefix gnu:)
  #:export (%standard-phases
            nix-rust-build))

(define* (configure #:key inputs rust-target cc env-vars #:allow-other-keys)
  (let ((vendor (assoc-ref inputs "vendor")))
    
    (mkdir-p ".cargo")
    (call-with-output-file ".cargo/config.toml"
      (lambda (cc-toml)
        (display "[source.cargo]\n" cc-toml)
        (format cc-toml "directory = ~s\n" vendor)
        (display "[source.crates-io]\n" cc-toml)
        (display "replace-with = 'cargo'\n" cc-toml)
        (format cc-toml "[target.~a]\n" rust-target)
        (format cc-toml "linker = ~s\n" cc)))

    (setenv "CC" cc)
    (setenv "CARGO_HOME" (string-append (getenv "TMPDIR") "/.cargo"))
    (setenv "CARGO_VENDOR_DIR" vendor)
    (invoke "cargo" "update" "--manifest-path" "Cargo.toml" "--verbose")

    (for-each (lambda (env) (setenv (car env) (cdr env)))
              env-vars)))

(define* (build #:rest _)
  (invoke "cargo" "build"))

(define* (install #:key outputs #:allow-other-keys)
  (let ((out (assoc-ref outputs "out")))
    (mkdir-p out)
    (invoke "cargo" "install"
            "--path" "."
            "--root" out
            "--verbose")))

(define %standard-phases
  (modify-phases gnu:%standard-phases
    (delete 'bootstrap)
    (delete 'check)
    (replace 'configure configure)
    (replace 'build build)
    (replace 'install install)))

(define* (nix-rust-build #:key inputs (phases %standard-phases) #:allow-other-keys #:rest args)
  (apply gnu:gnu-build #:inputs inputs #:phases phases args))
