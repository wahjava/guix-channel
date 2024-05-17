(define-module (abbe packages git)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages version-control)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module ((guix licenses) :prefix license:))

(define-public git-2-45-1
  (let* ((version "2.45.1")
	 (git-manpages (origin
			  (method url-fetch)
			  (uri (string-append
				"mirror://kernel.org/software/scm/git/git-manpages-"
				version ".tar.xz"))
			  (sha256
			   (base32
			    "1w6r2liifafsxydmc48p578z7z70ys0spm6qp5ygdd0l26mxf8p6")))))
    (package
      (inherit git)
      (name "git-2-45-1")
      (version version)
      (source (origin
		(method url-fetch)
		(uri (string-append "mirror://kernel.org/software/scm/git/git-"
                                    version ".tar.xz"))
		(sha256
		 (base32
		  "1gqj5xrlmzs4amrj7xgxx7qpqj8br8f6bk4bzcnf4yk2iq538kg6"))))
      (native-inputs (modify-inputs (package-native-inputs git)
		       (replace "git-manpages" git-manpages))))))
