(define-module (abbe packages zsh)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module  (guix build-system copy)
  #:use-module  (guix git-download)
  #:use-module (guix git))

(define-public oh-my-zsh
  (let ((rev "6e9d57d5abdb33b0c22f8979dc529c1a046d7aa3"))
    (package
      (name "oh-my-zsh")
      (version "20240411")
      (source
       (origin (method git-fetch)
               (uri (git-reference
                     (url "https://github.com/ohmyzsh/ohmyzsh")
                     (commit rev)))
               (file-name (git-file-name name version))
               (sha256 (base32 "0mwl1zdn53k4157zgswgr9yhdyjcrp54f79qxmkpzv4gdf3cvxd2"))))
      (build-system copy-build-system)
      (home-page "https://ohmyz.sh/")
      (synopsis "Oh My Zsh configuration framework")
      (description
       "This package provides Oh My Zsh configuration framework for zsh.")
      (license license:expat-0)
      (arguments (list #:install-plan
                       #~(cons (list "oh-my-zsh.sh" "share/zsh/plugins/oh-my-zsh/oh-my-zsh.zsh")
                               (map (lambda (d) `(,d "share/zsh/plugins/oh-my-zsh/"))
				    '("cache" "custom""lib" "log" "plugins"
				      "templates" "themes""tools"))))))))

(define-public powerlevel-10k
  (package
   (name "powerlevel-10k")
   (version "1.20.0")
   (source
    (origin (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/romkatv/powerlevel10k")
                  (commit (string-append "v" version))))
            (file-name (git-file-name name version))
            (sha256 (base32 "1ha7qb601mk97lxvcj9dmbypwx7z5v0b7mkqahzsq073f4jnybhi"))))
   (build-system copy-build-system)
   (home-page "https://github.com/romkatv/powerlevel10k/")
   (synopsis "Powerlevel 10K zsh prompt framework")
   (description
    "This package provides powerlevel10k prompt framework for zsh.")
   (license license:expat-0)
   (arguments (list #:install-plan
                    #~(map (lambda (d)
                             (list d
                                   "share/zsh/plugins/powerlevel-10k/"))
                           (list "config"
                                 "gitstatus"
                                 "internal"
                                 "Makefile"
                                 "powerlevel10k.zsh-theme"
                                 "powerlevel9k.zsh-theme"
                                 "prompt_powerlevel10k_setup"
                                 "prompt_powerlevel9k_setup"))))))
