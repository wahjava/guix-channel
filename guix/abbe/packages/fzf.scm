(define-module (abbe packages fzf)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (abbe build-system nix-go)
  #:use-module (guix git-download)
  #:use-module (guix licenses))

(define %empty-hash (make-string 52 #\0))

(define-public fzf
  (package
   (name "fzf")
   (version "0.55.0")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/junegunn/fzf")
                  (commit (string-append "v" version))))
            (sha256 (base32 "0s4df9b2bbf4g32d6yhbdzkzhxmcw3fgl3m1j4fvl5sbx84hsag2"))))
   (build-system nix-go-build-system)
   (arguments
    `(#:vendor-hash "09h5yxh6m9myggb658wfsn8lq43bb1j4prjbn3srl43z69f45f3g"
      #:build-flags ("-a" "-trimpath")
      #:ldflags (list "-s" "-w" "-X"
                       ,(string-append  "main.version=" version)
                       "-X"
                       "main.revision=fc693080")
      #:phases
      ,#~(modify-phases %standard-phases
          (add-after 'install 'install-manpages-et-al
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (bindir (string-append out "/bin"))
                     (man1 (string-append out "/share/man/man1")))

                (install-file "man/man1/fzf.1" man1)
                (install-file "man/man1/fzf-tmux.1" man1)

                (install-file "bin/fzf-tmux" bindir)
                (install-file "bin/fzf-preview.sh" bindir)))))))

   (synopsis "A command-line fuzzy finder")
   (description "A command-line fuzzy finder")
   (home-page "https://junegunn.github.io/fzf/")
   (license expat)))
