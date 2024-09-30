(define-module (abbe packages senpai)
  #:use-module (guix packages)
  #:use-module (gnu packages man)
  #:use-module (guix gexp)
  #:use-module (guix build utils)
  #:use-module (abbe build-system nix-go)
  #:use-module (guix git-download)
  #:use-module (rnrs io ports)
  #:use-module ((guix licenses) :prefix license:))

(define-public senpai
  (package
    (name "senpai")
    (version "0.3.0.20240814")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://git.sr.ht/~delthas/senpai")
                    (commit "752eb5c8b538d30bd1c1bd20c019a43367b4ba42")))
              (file-name (git-file-name name version))
              (sha256 (base32 "17pscprxi0iyxjd0d9cng00m2q6vasqnbpx0vlxx8f9napb9rj38"))))
    (build-system nix-go-build-system)
    (home-page "https://git.sr.ht/~delthas/senpai/")
    (synopsis "senpai is an IRC client.")
    (description
     "senpai is an IRC client that works best with bouncers")
    (license license:expat)
    (native-inputs (list scdoc))
    (arguments
     `(#:sub-packages ("./cmd/senpai")
       #:vendor-hash "1b8cp0g83lv8mg6381m9vrvf1sdb5fmilyci7p3q4d2q3ssvkhgc"
       #:phases
       ,#~(modify-phases %standard-phases
            (add-after 'build 'build-manpages
              (lambda* _
                (let ((scdoc (lambda  (in-file out-file)
                               (call-with-input-file in-file
                                 (lambda (in-port)
                                   (call-with-output-file out-file
                                     (lambda (out-port)
                                       (waitpid (spawn "scdoc" '("scdoc")
                                                       #:search-path? #t
                                                       #:input in-port
                                                       #:output out-port)))))))))
                  (scdoc "doc/senpai.1.scd" "doc/senpai.1")
                  (scdoc "doc/senpai.5.scd" "doc/senpai.5"))))
            (add-after 'install 'install-manpages
              (lambda* (#:key outputs #:allow-other-keys)
                (let* ((out (assoc-ref outputs "out"))
                       (mandir (string-append out "/share/man")))
                  (invoke "ls" "-la" "doc/")
                  (install-file "doc/senpai.1"
                                (string-append mandir "/man1"))
                  (install-file "doc/senpai.5"
                                (string-append mandir "/man5"))))))))))
