(define-module (abbe packages yazi)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (nonguix build-system binary)
  #:use-module (guix download)
  #:use-module (ice-9 match)
  #:use-module (guix licenses))

(define yazi-version "0.2.5")

(define (yazi-hash)
  (match (%current-system)
    ("aarch64-linux" "1vpgnpz45gw5b7inpdnj7ns005645v19pbrdmn11sg4ff7lrlr0a")
    ("x86_64-linux" "09mdfrlwx86k8fymxjjnxilxhwfp0g9vx452ybkqc8y4mjls2wxn")))

(define (yazi-filename)
  (match (%current-system)
    ("aarch64-linux" "yazi-aarch64-unknown-linux-gnu.zip")
    ("x86_64-linux"  "yazi-x86_64-unknown-linux-gnu.zip")))

(define-public yazi
  (package
    (name "yazi")
    (version yazi-version)
    (source (origin
              (method url-fetch)
              (uri (string-append
                     "https://github.com/sxyazi/yazi/releases/download/v"
                     version "/" (yazi-filename)))
	      (sha256 (base32 (yazi-hash)))))
    (build-system binary-build-system)
    (native-inputs (list unzip))
    (arguments
     `(#:patchelf-plan
       `(("ya" ("glibc" "gcc"))
         ("yazi" ("glibc" "gcc")))
       #:install-plan
       ,#~(list
	        (list "ya" "bin/ya")
	        (list "yazi" "bin/yazi")
            (list "completions/ya.bash" "etc/bash_compleition.d/")
            (list "completions/yazi.bash" "etc/bash_compleition.d/")
            (list "completions/_ya" "share/zsh/site-functions/")
            (list "completions/_yazi" "share/zsh/site-functions/"))))
	(inputs
     (list
      glibc
      `(,gcc "lib")))
    (synopsis "Blazing fast terminal file manager written in Rust, based on async I/O.")
    (description "Yazi (means \"duck\") is a terminal file manager written in Rust, based on non-blocking async I/O. It aims to provide an efficient, user-friendly, and customizable file management experience.")
    (home-page "https://yazi-rs.github.io/")
    (license expat)))
