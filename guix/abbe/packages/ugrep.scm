(define-module (abbe packages ugrep)
  #:use-module (guix packages)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages search)
  #:use-module (guix gexp)
  #:use-module (guix git-download))

(define-public ugrep-6
  (package
    (inherit ugrep)
    (name "ugrep")
    (version "6.3.0")
    (inputs (modify-inputs (package-inputs ugrep)
			   (append brotli)))
    (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/Genivia/ugrep")
                      (commit (string-append "v" version))))
                (sha256 (base32 
			 "0s4cic232nlqzr3ha40gfgdnfqj12j6k5j89mrzjv8gk2zwn9l5z"))
		(file-name (git-file-name name version))
                (modules '((guix build utils)))
                (snippet
                 #~(begin
		     (delete-file-recursively "bin/win32") ; pre-built 
		     (delete-file-recursively "bin/win64") ; pre-built executables
		     ))))))
