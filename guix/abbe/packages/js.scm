(define-module (abbe packages js)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (ice-9 match)
  #:use-module (guix base16)
  #:use-module (nonguix build-system binary)
  #:use-module ((gnu packages compression) #:select (unzip))
  #:use-module ((gnu packages gcc) #:select (gcc))
  #:use-module ((gnu packages base) #:select (glibc))
  #:use-module ((guix licenses) #:prefix license:))

(define bun-version "1.1.24")
(define deno-version "1.45.5")

(define (bun-url)
  (match (%current-system)
    ("aarch64-linux" (string-append "https://github.com/oven-sh/bun/releases/download/bun-v"
                                    bun-version
                                    "/bun-linux-aarch64.zip"))
    ("x86_64-linux" (string-append "https://github.com/oven-sh/bun/releases/download/bun-v"
                                   bun-version
                                   "/bun-linux-x64.zip"))))
(define (deno-url)
  (match (%current-system)
    ("aarch64-linux" (string-append "https://github.com/denoland/deno/releases/download/v"
                                    deno-version "/deno-aarch64-unknown-linux-gnu.zip"))
    ("x86_64-linux" (string-append "https://github.com/denoland/deno/releases/download/v"
                                    deno-version "/deno-x86_64-unknown-linux-gnu.zip"))))
(define (bun-hash)
  (match (%current-system)
    ("aarch64-linux" "82e73030cb3b6399ea84fcd8603a034783f357c6af7d7959e9a7bcafbc1f66a0")
    ("x86_64-linux" "f478880678cf282fdda46cc9d58e20a0dde1fad8c9cfee128ead43996453e1c8")))

(define (deno-hash)
  (match (%current-system)
    ("aarch64-linux" "192i734a6dnygglycnrlfyapnl7i28na0qzqd6lsm5z50z6pyshg")
    ("x86_64-linux" "0a9bdxcl4qmmlgl7nmrbllqfln9yj0rfmaw7ly92j8c9i0y7ckz5")))

(define-public bun
  (package
    (name "bun")
    (version bun-version)
    (source (origin (method url-fetch)
                    (uri (bun-url))
                    (sha256 (base16-string->bytevector (bun-hash)))))
    (native-inputs (list unzip))
    (build-system binary-build-system)
    (arguments
     `(#:install-plan
       '(("bun" "bin/"))
       #:patchelf-plan
       '(("bun"))))
    (home-page "https://bun.sh")
    (description "Bun is an all-in-one toolkit for JavaScript and TypeScript apps. It ships as a single executable called bun.")
    (synopsis "Incredibly fast JavaScript runtime, bundler, test runner, and package manager - all in one")
    (license license:expat)))

(define-public deno
  (package
    (name "deno")
    (version deno-version)
    (source (origin (method url-fetch)
                    (uri (deno-url))
                    (sha256 (base32 (deno-hash)))))
    (inputs (list (list gcc "lib") glibc))
    (native-inputs (list unzip))
    (build-system binary-build-system)
    (arguments
     `(#:install-plan
       '(("deno" "bin/"))
       #:patchelf-plan
       '(("deno")
         ("deno" ("gcc" "glibc")))))
    (home-page "https://deno.com")
    (description "Deno is the open-source JavaScript runtime for the modern web. Built on web standards with zero-config TypeScript, unmatched security, and a complete built-in toolchain, Deno is the easiest, most productive way to JavaScript.")
    (synopsis "Deno is the open-source JavaScript runtime for the modern web.")
    (license license:expat)))
