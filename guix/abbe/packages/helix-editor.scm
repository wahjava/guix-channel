(define-module (abbe packages helix-editor)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (guix gexp)
  #:use-module (nonguix build-system binary)
  #:use-module (guix download)
  #:use-module (guix licenses))

(define helix-version "24.03")
(define helix-base-dir ".")

(define-public helix-editor
  (package
   (name "helix-editor")
   (version helix-version)
   (source (origin
            (method url-fetch)
            (uri (string-append
		  "https://github.com/helix-editor/helix/releases/download/"
		  version
		  "/helix-"
		  version
		  "-aarch64-linux.tar.xz"))
	    (sha256 (base32 "14nlz3yxjf27xd7rr42iapk99y1z3bh0cjlfn70ac3hz1kzfkany"))))
   (build-system binary-build-system)
   (arguments
    `(#:patchelf-plan
      `(("hx" ("glibc" "gcc")))
      #:install-plan
      ,#~(list
	  (list (string-append #$helix-base-dir "/hx") "bin/hx")
	  (list (string-append #$helix-base-dir "/runtime") "share/helix/")
	  (list (string-append #$helix-base-dir "/contrib") "share/helix/"))))
   (inputs
    (list
     glibc
     `(,gcc "lib")))
   (synopsis "Helix is an editor")
   (description "Helix is an editor")
   (home-page "https://helix-editor.com/")
   (license mpl2.0)))
