(define-module (abbe packages neovim)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (gnu packages vim)
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

