(define-module (abbe build nix-go-build-system)
  #:use-module (guix build utils)
  #:use-module ((guix build gnu-build-system) #:prefix gnu:)
  #:use-module (srfi srfi-1)
  #:export (%standard-phases
            nix-go-build))

(define* (setup-nix-go-environment #:key goos goarch #:allow-other-keys)
  (let ((tmpdir (or (getenv "TMPDIR") "/tmp")))
    (setenv "TMPDIR" "/tmp")
    (setenv "GOPATH" (string-append tmpdir "/go"))
    (setenv "GOCACHE" (string-append tmpdir "/go-cache"))
    (setenv "GOPROXY" "off")
    (setenv "GOSUMDB" "off")
    (setenv "CGO_ENABLED" "0")
    (setenv "GO111MODULE" "on")
    (setenv "GOTOOLCHAIN" "local")
    (setenv "GOOS" goos)
    (setenv "GOARCH" goarch)))

(define* (symlink-vendor #:key inputs #:allow-other-keys)
  (invoke "rm" "-rf" "vendor")
  (symlink (assoc-ref inputs "vendor") "vendor"))

(define* (build #:key ldflags tags build-flags sub-packages #:allow-other-keys)
  (let ((tags (if (list? tags)
                  (list (string-append "-tags="
                                       (string-join tags ",")))
                  (list)))
        (ldflags (list "-ldflags"
                       (if (list? ldflags)
                           (string-join (if (every (lambda (el)
                                                     (not (string-prefix? "-buildid=" el))) ldflags)
                                            (cons "-buildid=" ldflags)
                                            ldflags) " ")
                           "-buildid="))))
    (for-each (lambda (pkg)
                (apply invoke
                       `("go" "install" ,@build-flags ,@tags ,@ldflags ,pkg)))
              sub-packages)))

(define* (install #:key outputs #:allow-other-keys)
  (let* ((out (assoc-ref outputs "out"))
	       (bindir (string-append out "/bin"))
	       (gobin (string-append (getenv "GOPATH") "/bin")))
    (mkdir-p out)
    (when (directory-exists? gobin)
      (mkdir-p bindir)
      (copy-recursively gobin bindir))))

(define* (nix-go-build #:key inputs (phases %standard-phases)
                       #:allow-other-keys #:rest args)
  "Build the given Go package, applying all of PHASES in order."
  (apply gnu:gnu-build #:inputs inputs #:phases phases args))

(define %standard-phases
  (modify-phases gnu:%standard-phases
    (delete 'bootstrap)
    (delete 'configure)
    (delete 'patch-generated-file-shebangs)
    (delete 'check)
    (replace 'build build)
    (replace 'install install)
    (add-before 'unpack 'setup-nix-go-environment setup-nix-go-environment)
    (add-after 'patch-source-shebangs 'symlink-vendor symlink-vendor)))
