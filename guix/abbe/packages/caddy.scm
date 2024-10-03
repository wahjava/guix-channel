(define-module (abbe packages caddy)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (abbe packages go)
  #:use-module (abbe build-system nix-go)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) :prefix license:)
  #:use-module (ice-9 match))

(define-public caddy
  (package
   (name "caddy")
   (version "2.8.4")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/caddyserver/caddy")
                  (commit (string-append "v" version))))
            (sha256 (base32 "0i3y2w36dvif5jnvpv4c31l1hw1vsyvwb206ccn0dpm9snmg45q8"))))
   (build-system nix-go-build-system)
   (arguments
    (list
     #:go go-122
     #:sub-packages '("./cmd/caddy")
     #:ldflags `(list "-X"
                 ,(string-append "github.com/caddyserver/caddy/v2.CustomVersion=v" version))
     #:vendor-hash "0lw97bcidr55n2hvzx6apilan92qn241h6afcbl5y9srn3qn42nl"
     #:phases
     #~(modify-phases %standard-phases
         (add-after 'install 'install-manpages-et-al
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (mandir (string-append out "/share/man/man8"))
                    (caddy-exe (string-append bin "/caddy"))
                    (bashcomp (string-append out "/share/bash-completion/completions")))
               (mkdir-p mandir)
               (mkdir-p bashcomp)
               (invoke caddy-exe "manpage" "--directory" mandir)
               (call-with-output-file (string-append bashcomp "/caddy")
                 (lambda (out-port)
                   (waitpid (spawn caddy-exe (list caddy-exe "completion" "bash")
                                   #:output out-port))))))))))
   (synopsis "Caddy is a web server")
   (description
    "Fast and extensible multi-platform HTTP/1-2-3 web server with automatic HTTPS")
   (home-page "https://caddyserver.com/")
   (license license:asl2.0)))
