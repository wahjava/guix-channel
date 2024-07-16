(define-module (abbe packages zsh)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module  (guix build-system copy)
  #:use-module  (guix git-download)
  #:use-module (guix git))

(define-public oh-my-zsh
  (let ((rev "be10a9127731e7c6a9a57f8c8c1789e233117ae9"))
    (package
      (name "oh-my-zsh")
      (version "20240715")
      (source
       (origin (method git-fetch)
               (uri (git-reference
                     (url "https://github.com/ohmyzsh/ohmyzsh")
                     (commit rev)))
               (file-name (git-file-name name version))
               (sha256 (base32 "1mh3ggswgv4gzpsjbki36cynwjxcwqcdbcbyydn0gmdw283cs418"))))
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

(define %powerlevel-10k-rev "2b7da93df04acd04d84f5de827e5b14077839a4b")

(define-public powerlevel-10k
  (package
   (name "powerlevel-10k")
   (version "1.20.0-20240701")
   (source
    (origin (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/romkatv/powerlevel10k")
                  (commit %powerlevel-10k-rev)))
            (file-name (git-file-name name version))
            (sha256 (base32 "06qx1dq3vgp5ly8k425ai10axrx2g2abfq52chcm61y1nb2dcclg"))))
   (build-system copy-build-system)
   (home-page "https://github.com/romkatv/powerlevel10k/")
   (synopsis "Powerlevel 10K zsh prompt framework")
   (description
    "This package provides powerlevel10k prompt framework for zsh.")
   (license license:expat-0)
   (arguments (list #:install-plan
                    #~(map (lambda (d)
                             (list d
                                   "share/zsh/plugins/powerlevel10k/"))
                           (list "config"
                                 "gitstatus"
                                 "internal"
                                 "Makefile"
                                 "powerlevel10k.zsh-theme"
                                 "powerlevel9k.zsh-theme"
                                 "prompt_powerlevel10k_setup"
                                 "prompt_powerlevel9k_setup"))))))

(define-public zsh-history-filter
  (package
   (name "zsh-history-filter")
   (version "0.4.1-20210405")
   (source
    (origin (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/MichaelAquilina/zsh-history-filter")
                  (commit "49d7987f093374fb72f0f7eef2d63e641085a226")))
            (file-name (git-file-name name version))
            (sha256 (base32 "1avkhh7dhbw3kijvhs6xs8b4xc273xprsxrhmsmxh4ha7vibyahd"))))
   (build-system copy-build-system)
   (home-page "https://github.com/MichaelAquilina/zsh-history-filter/")
   (synopsis "Zsh plugin to filter out some commands from being added to you history")
   (description
    "zsh plugin that allows you to specify patterns that will automatically exclude commands from being inserted into your permanent history. Particularly useful for preventing secrets being written.")
   (license license:gpl3)
   (arguments (list #:install-plan
                    #~(map (lambda (d)
                             (list d
                                   "share/zsh/plugins/zsh-history-filter/"))
                           (list "README.rst"
                                 "zsh-history-filter.plugin.zsh"))))))
