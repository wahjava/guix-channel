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
  #:use-module (abbe packages tree-sitter))

(define-public neovim-0-10
  (package
    (inherit neovim)
    (name "neovim")
    (version "0.10.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/neovim/neovim")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0r5mjfsgrllxi44i9k6lb8b99rpzrwhkg18aiqmby8wwzflbqdy3"))))
    (inputs (modify-inputs (package-inputs neovim)
             (replace "tree-sitter" tree-sitter)))))

(define-public neovim-bin
  (package
    (inherit neovim)
    (name "neovim-bin")
    (version "0.10.1")
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
             (sha256 (base16-string->bytevector "4867de01a17f6083f902f8aa5215b40b0ed3a36e83cc0293de3f11708f1f9793"))))))
