(define-module (abbe build-system nix-go)
  #:use-module (guix build-system)
  #:use-module (guix build utils)
  #:use-module (guix base32)
  #:use-module (guix gexp)
  #:use-module (guix monads)
  #:use-module (guix store)
  #:use-module (guix utils)
  #:use-module (guix modules)
  #:use-module (guix search-paths)
  #:use-module ((guix packages) #:prefix pkgs:)
  #:use-module ((guix build gnu-build-system) #:prefix gnu:)
  #:use-module ((gnu packages certs) #:select (nss-certs))
  #:use-module ((gnu packages version-control) #:select (git-minimal))
  #:use-module (guix build-system gnu)
  #:export (nix-go-build-system
            nix-go-build
            %standard-phases))

(define %nix-go-build-system-modules
  `((guix build utils)
    ;(guix gexp)
    ;(guix build-system)
    (abbe build nix-go-build-system)
    ,@%default-gnu-imported-modules))

(define (default-go)
  ;; Lazily resolve the binding to avoid a circular dependency.
  (let ((go (resolve-interface '(abbe packages go))))
    (module-ref go 'go-122)))

(define* (lower name
                #:key source inputs native-inputs outputs system target
                (go #f)
                (vendor-hash #f)
                (ldflags #f)
                #:allow-other-keys
                #:rest arguments)
  "Return a bag for NAME."

  (define private-keywords
    '(#:target #:go #:inputs #:native-inputs #:vendor-hash))

  (define* (go-vendor-fetch url hash-algo hash name #:key system)
    (let ((pkg (car url))
          (go-toolchain (cdr url)))
      (gexp->derivation
       (string-append name "-vendor")
       #~(begin
           (let ((tmpdir (or (getenv "TMPDIR") "/tmp")))
             (use-modules (guix build utils)
                          (guix search-paths))

             (setenv "PATH" (cdar (evaluate-search-paths (list $PATH)
                                                         (list
                                                          #$git-minimal #$go-toolchain
                                                          #$@(map cadr (standard-packages))))))

             (setenv "SSL_CERT_DIR" (string-append #$nss-certs "/etc/ssl/certs"))
             (setenv "GOPATH" (string-append tmpdir "/go"))
             (setenv "GOCACHE" (string-append tmpdir "/go-cache"))
             (setenv "GOPROXY" "")
             (setenv "GO111MODULE" "on")
             (setenv "GOTOOLCHAIN" "local")
             (setenv "CGO_ENABLED" "0")

             (invoke "cp" "-r" "--reflink=auto" #$source "modRoot")
             (chdir "modRoot")
             (chmod "." #o755)

             (invoke "go" "mod" "vendor")
             (mkdir-p "vendor")

             (invoke "cp" "-r" "--reflink=auto" "vendor" #$output)))

       #:modules
       '((guix build utils)
         (guix records)
         (guix search-paths))
       #:local-build? #t
       #:recursive? #t
       #:system system
       #:hash-algo hash-algo
       #:hash hash
       #:leaked-env-vars '("http_proxy" "https_proxy" "LC_ALL" "LC_MESSAGES" "LANG" "COLUMNS"))))

  (define (vendor-origin name vendor-hash go-toolchain)
    (pkgs:origin (method go-vendor-fetch)
                 (uri (cons source go-toolchain))
                 (file-name name)
                 (hash (pkgs:content-hash (nix-base32-string->bytevector vendor-hash) sha256))))

  (define go-toolchain
    (if go
        go
        (default-go)))

  (bag
    (name name)
    (system system)
    (target target)
    (build-inputs `(,@(if source
                          `(("source" ,source))
                          '())
                    ,@(if vendor-hash
                          `(("vendor" ,(vendor-origin name vendor-hash go-toolchain)))
                          '())
                    ,@`(("go" ,go-toolchain))
                    ,@native-inputs
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
    (build nix-go-build)
    (arguments (strip-keyword-arguments private-keywords arguments))))

(define* (nix-go-build name inputs
                       #:key
                       source
                       (phases '%standard-phases)
                       (outputs '("out"))
                       (search-paths '())
                       (install-source? #f)
                       (build-flags ''())
                       (tests? #t)
                       (parallel-build? #t)
                       (parallel-tests? #t)
                       (allow-go-reference? #f)
                       (system (%current-system))
                       (go (default-go))
                       (goarch #f)
                       (goos #f)
                       (ldflags #f)
                       (tags #f)
                       (sub-packages '("."))
                       (guile #f)
                       (imported-modules %nix-go-build-system-modules)
                       (modules '((abbe build nix-go-build-system)
                                  (guix build utils)))

                       (substitutable? #t))

  (define builder
    (with-imported-modules imported-modules
      #~(begin
          (use-modules #$@(sexp->gexp modules))
          (nix-go-build #:name #$name
                        #:source #+source
                        #:system #$system
                        #:phases #$phases
                        #:outputs #$(outputs->gexp outputs)
                        #:substitutable? #$substitutable?
                        #:go #$go
                        #:goarch #$goarch
                        #:goos #$goos
                        #:ldflags #$ldflags
                        #:tags #$tags
                        #:sub-packages (list #$@sub-packages)
                        #:search-paths '#$(sexp->gexp
                                           (map search-path-specification->sexp
                                                search-paths))
                        #:install-source? #$install-source?
                        #:build-flags #$build-flags
                        #:tests? #$tests?
                        #:parallel-build? #$parallel-build?
                        #:parallel-tests? #$parallel-tests?
                        #:allow-go-reference? #$allow-go-reference?
                        #:inputs #$(input-tuples->gexp inputs)))))

  (mlet %store-monad ((guile (pkgs:package->derivation (or guile (pkgs:default-guile))
                                                       system #:graft? #f)))
    (gexp->derivation name builder
                      #:system system
                      #:guile-for-build guile)))

(define nix-go-build-system
  (build-system
    (name 'nix-go)
    (description
     "Build system for Go programs")
    (lower lower)))

