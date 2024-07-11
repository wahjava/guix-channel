(define-module (abbe packages zed)
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

(define zed-version "0.143.6")

(define (zed-hash)
  (match (%current-system)
    ("aarch64-linux" "0wgfqp9m1dcgh9nhnh6bcxkix25h84pjn9w1qb7q94wnr8ir05d2")
    ("x86_64-linux" "1q4ghd9qm4ing38gjry7qdrfyynya66zis554rd1gl76mm6l4n4f")))

(define (zed-arch)
  (match (%current-system)
    ("aarch64-linux" "aarch64")
    ("x86_64-linux" "x86_64")))

(define-public zed-editor
  (package
   (name "zed-editor")
   (version zed-version)
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://github.com/zed-industries/zed/releases/download/v"
                  version
                  "/zed-linux-"
                  (zed-arch)
                  ".tar.gz"))
            (sha256 (base32 (zed-hash)))))
   (build-system binary-build-system)
   (arguments
    `(#:patchelf-plan
      `(("bin/zed" ("glibc" "gcc"))
        ("libexec/zed-editor" ("out" "gcc" "glibc"))
        ("lib/libXdmcp.so.6" ("out"))
        ("lib/libssl.so.1.1" ("out"))
        ("lib/libxcb.so.1" ("out"))
        ("lib/libxcb-xkb.so.1" ("out"))
        ("lib/libxkbcommon-x11.so.0" ("out"))
        ("lib/libasound.so.2" ("glibc")))))
   (inputs
    (list
     glibc
     `(,gcc "lib")))
   (synopsis "zed is an editor")
   (description "zed is an editor")
   (home-page "https://zed.dev/")
   (license gpl3+)))
