(define-module (abbe packages yazi)
  #:use-module (gnu packages)
  #:use-module (gnu packages jemalloc)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (abbe build-system nix-rust)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:))

(define jemalloc/patched
  (package/inherit jemalloc
    (arguments
     (substitute-keyword-arguments (package-arguments jemalloc)
       ((#:configure-flags flags (list))
        #~(cons "--with-jemalloc-prefix=_rjem_" #$flags))))))

(define-public yazi
  (package
    (name "yazi")
    (version "0.3.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference (url "https://github.com/sxyazi/yazi")
                                  (commit (string-append "v" version))))
	            (sha256 (base32 "08bcyi1jhrip0q2drrkmcfcfbll0aq5nm56fz6w42dw9dgrdyc3d"))))
    (build-system nix-rust-build-system)
    (inputs (list jemalloc/patched))
    (arguments
     `(#:vendor-hash "12bjkynqikd8n37br78k37fa0xwh8r9bvs4mwk05f4x49invhzlr"
       #:cargo-install-paths ("./yazi-cli" "./yazi-fm")
       #:env-vars
       (list '("VERGEN_GIT_SHA" . "7c445cef1fd9ff9d41314d3d2b02cbc74250c6bf")
             '("VERGEN_GIT_COMMIT_DATE" . "2024-09-04T15:16:18Z")
             '("YAZI_GEN_COMPLETIONS" . "true")
             '("JEMALLOC_SYS_WITH_MALLOC_CONF" . "narenas:1")
             (cons "JEMALLOC_OVERRIDE"
                   ,#~(string-append #$@(assoc-ref (package-inputs this-package) "jemalloc")
                                     "/lib/libjemalloc.so")))
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-shell-completions
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((comps '(("_yazi" . "share/zsh/site-functions/_yazi")
                            ("yazi.bash" . "share/bash-completion/completions/yazi")
                            ("yazi.fish" . "share/fish/vendor_completions.d/yazi.fish")))
                   (out (assoc-ref outputs "out")))
               (for-each (lambda (file)
                           (install-file
                            (string-append "./yazi-boot/completions/" (car file))
                            (string-append out "/" (cdr file))))
                         comps)))))))

    (synopsis "Blazing fast terminal file manager written in Rust, based on async I/O.")
    (description "Yazi (means \"duck\") is a terminal file manager written in Rust, based on non-blocking async I/O. It aims to provide an efficient, user-friendly, and customizable file management experience.")
    (home-page "https://yazi-rs.github.io/")
    (license license:expat)))
