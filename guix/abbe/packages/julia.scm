(define-module (abbe packages julia)
  #:use-module (guix packages)
  #:use-module (nonguix build-system binary)
  #:use-module (guix download)
  #:use-module (guix licenses)
;  #:use-module (gnu packages tls)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages base)
  #:use-module (guix base16)
  #:use-module (ice-9 match))

(define (major-version version)
  (match (string-split version #\.)
    ((v1 v2 v3)
     (string-append v1 "." v2))))

(define-public julia-bin
  (package
   (name "julia")
   (version "1.10.5")
   (source (origin
            (method url-fetch)
            (uri (string-append "https://julialang-s3.julialang.org/bin/linux/x64/"
                                  				(major-version version)
                                  				"/julia-" version "-linux-x86_64.tar.gz"))
            (sha256 (base16-string->bytevector "33497b93cf9dd65e8431024fd1db19cbfbe30bd796775a59d53e2df9a8de6dc0"))))
   (build-system binary-build-system)
   (arguments
    (list
     #:strip-binaries? #f
     #:patchelf-plan
      `'(("lib/julia/libLLVM-15jl.so" (("out" "/lib/julia") "glibc"))
         ("lib/julia/libcholmod.so"   (("out" "/lib/julia") "glibc"))
         ("lib/julia/libgfortran.so"  (("out" "/lib/julia") "glibc"))
         ("lib/julia/libmpfr.so"  (("out" "/lib/julia") "glibc"))
         ("lib/julia/libgit2.so"      (("out" "/lib/julia")))
         ("lib/julia/libjulia-internal.so" (("out" "/lib/julia") "out" "glibc"))
         ("lib/julia/libllvmcalltest.so"   (("out" "/lib/julia") "glibc"))
         ("lib/julia/libopenblas64_.0.3.23.so" (("out" "/lib/julia") "glibc"))
         ("lib/julia/libstdc++.so"             (("out" "/lib/julia") "glibc"))
         ("lib/julia/libunwind.so"             (("out" "/lib/julia") "glibc"))
         ("lib/julia/sys.so"                   (("out" "/lib/julia") "out" "glibc"))

         ("bin/julia"              ("glibc" "out"))
         ("libexec/julia/7z"       ("glibc" ("out" "/lib/julia")))
         ("libexec/julia/dsymutil" ("glibc" ("out" "/lib/julia")))
         ("libexec/julia/lld"      ("glibc" ("out" "/lib/julia"))))))

   (inputs (list glibc))
   (synopsis "Julia language compiler")
   (description "Julia language compiler")
   (home-page "https://julialang.org/")
   (license expat)))
