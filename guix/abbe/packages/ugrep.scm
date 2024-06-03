(define-module (abbe packages ugrep)
  #:use-module (guix packages)
  #:use-module (gnu packages search)
  #:use-module (gnu packages bison)
  #:use-module (guix gexp)
  #:use-module (guix git-download))

(define-public ugrep-6
  (package
    (inherit ugrep)
    (name "ugrep")
    (version "6.1.0")
    (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/Genivia/ugrep")
                      (commit (string-append "v" version))))
                (sha256 (base32 
			 "01zgvwhawz1sv3sib31jgbs5q27yc4kqmhz3v9l1zbqkkhwxsvqy"))
		(file-name (git-file-name name version))
                (modules '((guix build utils)))
                (snippet
                 #~(begin
		     (delete-file-recursively "bin/win32") ; pre-built 
		     (delete-file-recursively "bin/win64") ; pre-built executables
		     (delete-file-recursively "bin/linux_amd64") ; pre-built
		     (delete-file-recursively "bin/linux_arm64") ; pre-built executables
		     ))))))
