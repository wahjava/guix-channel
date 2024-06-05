(define-module (abbe packages git)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages version-control)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module ((guix licenses) :prefix license:))

(define-public git-2-45-2
  (let* ((version "2.45.2")
	 (git-manpages (origin
			  (method url-fetch)
			  (uri (string-append
				"mirror://kernel.org/software/scm/git/git-manpages-"
				version ".tar.xz"))
			  (sha256
			   (base32
			    "1pqrp46kwbxycqld39027ph1cvkq9am156y3sswn6w2khsg30f09")))))
    (package
      (inherit git)
      (name "git")
      (version version)
      (source (origin
		(method url-fetch)
		(uri (string-append "mirror://kernel.org/software/scm/git/git-"
                                    version ".tar.xz"))
		(sha256
		 (base32
		  "1nws1vjgj54sv32wxl1h3n1jkcpabqv7a605hhafsby0n5zfigsi"))))
      (native-inputs (modify-inputs (package-native-inputs git)
		       (replace "git-manpages" git-manpages))))))
