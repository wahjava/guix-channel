(define-module (abbe packages gopass)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (abbe build-system nix-go)
  #:use-module (guix git-download)
  #:use-module (gnu packages golang)
  #:use-module ((guix licenses) #:prefix license:))

(define-public gopass
  (package
    (name "gopass")
    (version "1.15.14")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/gopasspw/gopass")
                    (commit (string-append "v" version))))
              (sha256 (base32 "1v7yjlhfr1wjqf3i685v7zv192lvhr3r08ja72gz3cmp6lgdv1fy"))))
    (build-system nix-go-build-system)
    (arguments
     `(#:go ,go-1.22
       #:vendor-hash "1gfwj3mcfsdlxaykq8yvv2qs9icjhxc7yld848ccj4qn49dnkshr"
       #:tags '("netgo")
       #:ldflags
       (list "-s" "-w"
             "-X"
             ,(string-append "main.version=" version)
             "-X"
             ,(string-append "main.commit=" version)
             "-X"
             "main.date=1970-01-01")
       #:phases
       ,#~(modify-phases %standard-phases
           (add-after 'install 'install-shell-completions
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((comps '(("zsh.completion" . "share/zsh/site-functions/_gopass")
	                            ("bash.completion" . "share/bash-completion/completions/gopass")
	                            ("fish.completion" . "share/fish/vendor_completions.d/gopass.fish")))
                     (out (assoc-ref outputs "out")))
                 (for-each (lambda (comp)
                             (let ((file (string-append out "/" (cdr comp))))
                               (mkdir-p (dirname file))
                               (copy-file (car comp)
                                          file)))
                           comps)))))))

     (synopsis "The slightly more awesome standard unix password manager for teams")
     (description "The slightly more awesome standard unix password manager for teams")
     (home-page "https://gopass.pw")
     (license license:expat)))
