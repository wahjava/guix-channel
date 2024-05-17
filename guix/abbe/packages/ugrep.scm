(define-module (abbe packages ugrep)
  #:use-module (guix packages)
  #:use-module (gnu packages search)
  #:use-module (gnu packages bison)
  #:use-module (guix gexp)
  #:use-module (guix git-download))

(define-public ugrep-6
  (package
    (inherit ugrep)
    (name "ugrep-6")
    (version "6.0.0")
    (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/Genivia/ugrep")
                      (commit (string-append "v" version))))
                (sha256 (base32 
			 "07lqjvyvda11d8xk1cmwwyx41w6l423jghcf21fnp4hrkrcsd5cd"))
		(file-name (git-file-name name version))
                (modules '((guix build utils)))
                (snippet
                 #~(begin
		     (delete-file-recursively "bin/win32") ; pre-built 
		     (delete-file-recursively "bin/win64") ; pre-built executables
		     ))))))
