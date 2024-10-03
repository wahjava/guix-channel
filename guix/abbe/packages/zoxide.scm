(define-module (abbe packages zoxide)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (abbe build-system nix-rust)
  #:use-module ((guix licenses) #:prefix license:))

(define-public zoxide
  (package
    (name "zoxide")
    (version "0.9.6")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/ajeetdsouza/zoxide")
                    (commit (string-append "v" version))))
              (sha256 (base32 "1p9dqszpc1jhinws5l4cgs8asbv2chiggkf32jldp2m5hcmvjw6x"))))
    (build-system nix-rust-build-system)
    (arguments
     `(#:vendor-hash "012qrqn09bx99h8zlk265vq39m3jimk24wa4ypnx4cg6s0zzi9zf"
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-manpages-et-al
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (comps '(("_zoxide" . "share/zsh/site-functions")
                            ("zoxide.fish" . "share/fish/vendor_completions.d")
                            ("zoxide.bash" . "share/bash-completion/completions")
                            ("zoxide.elv" . "share/elvish/lib"))))

               ;; install manpages
               (for-each (lambda (file)
                           (install-file file (string-append out "/share/man/man1")))
                         (find-files "man/man1" "\\.1$"))


               ;; install shell completions
               (for-each (lambda (file)
                           (install-file
                            (string-append "contrib/completions/" (car file))
                            (string-append out "/" (cdr file))))
                         comps) ))))))

    (synopsis "A smarter cd command. Supports all major shells.")
    (description "zoxide is a smarter cd command, inspired by z and autojump. It remembers which directories you use most frequently, so you can \"jump\" to them in just a few keystrokes. zoxide works on all major shells.
\"")
    (home-page "https://github.com/ajeetdsouza/zoxide")
    (license license:expat)))
