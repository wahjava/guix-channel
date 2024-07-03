(define-module (abbe packages neovim)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix base16)
  #:use-module (gnu packages vim)
  #:use-module (nonguix build-system binary)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages base)
  #:use-module (gnu packages tree-sitter))

(define-public tree-sitter-0-22-6
  (package
    (inherit tree-sitter)
    (name "tree-sitter")
    (version "0.22.6")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/tree-sitter/tree-sitter")
                    (commit "b40f342067a89cd6331bf4c27407588320f3c263")))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0nx3kmvbhjf6nq1pv6x85z67dkk5kagh90w1cwxh0pvg7608l44c"))
              (modules '((guix build utils)))
              (snippet #~(begin
                           ;; Remove bundled ICU parts
                           (delete-file-recursively "lib/src/unicode")))))))

(define-public neovim-0-10
  (package
    (inherit neovim)
    (name "neovim")
    (version "0.10.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/neovim/neovim")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0bd9hxnskyfg7k51yxf3gnqhv9l3y4m91yqd8svzj7d5sb2jxc1n"))))
    (inputs (modify-inputs (package-inputs neovim)
	      (replace "tree-sitter" tree-sitter-0-22-6)))))

(define-public neovim-bin
  (package
    (inherit neovim)
    (version "0.10.0")
    (build-system binary-build-system)
    (arguments
     `(#:patchelf-plan
       `(("bin/nvim" ("glibc" "gcc")))))
    (inputs
     (list
      glibc
      `(,gcc "lib")))
    (native-inputs '())
    (source (origin
	      (method url-fetch)
	      (uri (string-append
		     "https://github.com/neovim/neovim/releases/download/v" version "/nvim-linux64.tar.gz"))
	      (sha256 (base16-string->bytevector "be1f0988d0de71c375982b87b86cd28d2bab35ece8285abe3b0aac57604dfc5a"))))))
