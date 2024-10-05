(define-module (abbe packages helix-editor)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (abbe build-system nix-rust)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages compression)
  #:use-module (abbe packages helix-languages)
  #:use-module ((guix licenses) :prefix license:))

(define-public helix-editor
  (package
    (name "helix-editor")
    (version "24.07")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/helix-editor/helix")
                    (commit version)))
	            (sha256 (base32 "1f0l65z1cy8m9x79p5y5kwk1psv0ppfz9lwylggm71q0lj127awl"))
              (modules '((guix build utils)))
              (patch-flags '("-p0"))
              (patches (list (origin (method url-fetch)
                                     (uri "https://raw.githubusercontent.com/freebsd/freebsd-ports/89ab81d0e2eca6b298b0efd3f4577d468abf18d5/main/editors/helix/files/patch-helix-loader_src_grammar.rs")
                                     (file-name "helix-loader-src_Grammar.rs.diff")
                                     (sha256 (base32 "1wzvy9q4dirx10vzymz250ryw00q8q4sb4ykvnlhrc7dbdjlfymx")))
                             (origin (method url-fetch)
                                     (uri "https://raw.githubusercontent.com/freebsd/freebsd-ports/89ab81d0e2eca6b298b0efd3f4577d468abf18d5/main/editors/helix/files/patch-helix-loader_src_lib.rs")
                                     (file-name "helix-loader-src_lib.rs.diff")
                                     (sha256 (base32 "1v4ldbw50zjg9v89k4rmvscdn5mnijr96fj528qqk0lz3rlz41md")))))
              (snippet
               #~(begin
                   (let ((source-dir "runtime/grammars/sources"))
                     (setenv "PATH" (string-join (map (lambda (p)
                                                        (string-append p "/bin"))
                                                      (list #$coreutils #$tar #$gzip)) ":"))

                     (substitute* "helix-loader/src/lib.rs"
                       (("%%DATADIR%%")
                        (string-append #$output "/share/helix")))
                     (mkdir-p source-dir)
                     (for-each (lambda (f)
                                 (let ((grammar (cadr f)))
                                   (if (string-suffix? ".gz" grammar)
                                       (invoke "tar" "-xvf" grammar
                                               (string-append "--transform=s,^[^/]\\+/," (car f) "/,")
                                               "-C" source-dir)
                                       (symlink grammar
                                                (string-append source-dir "/" (car f))))))
                               (quote #$%helix-languages)))))))
    (build-system nix-rust-build-system)
    (native-inputs
     (list
      (list "pkg-config" pkg-config)))
    (arguments
     `(#:vendor-hash "1av3il4ajk3zlgc1k8x2ywvzp0hx6g3nc9da9yqh9qfbf1i8vvky"
       #:cargo-install-paths ("./helix-term")
       #:env-vars
       (list
        (cons "HELIX_DEFAULT_RUNTIME"
              (string-append (assoc-ref %outputs "out") "/lib/runtime")))
       #:phases
       (modify-phases %standard-phases

         (add-after 'install 'post-install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (libdir (string-append out "/lib"))
                    (comps '(("hx.bash" . "share/bash-completion/completions/hx")
                             ("hx.zsh" . "share/zsh/site-functions/_hx")
                             ("hx.fish" . "share/fish/vendor_completions.d/hx.fish")
                             ("hx.elv" . "share/elvish/lib/hx.elv"))))

               (delete-file-recursively "runtime/grammars/sources")

               (mkdir-p libdir)
               (invoke "cp" "-r" "--reflink=auto" "runtime" libdir)

               (mkdir-p (string-append out "/share/icons/hicolor/scalable/apps"))

               (copy-file "logo.svg"
                          (string-append out "/share/icons/hicolor/scalable/apps/helix.svg"))

               (install-file "contrib/helix.png"
                             (string-append out "/share/icons/hicolor/256x256/apps"))
               (install-file "contrib/Helix.desktop"
                             (string-append out "/share/applications"))

               (for-each (lambda (file)
                           (let ((full-path (string-append out (cdr file))))
                             (mkdir-p (dirname full-path))
                             (copy-file (string-append "contrib/completion/" (car file))
                                        full-path)))
                         comps)))))))

    (synopsis "Helix is an editor")
    (description "Helix is an editor")
    (home-page "https://helix-editor.com/")
    (license license:mpl2.0)))
