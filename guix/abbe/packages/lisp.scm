(define-module (abbe packages lisp)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (gnu packages lisp))

(define-public sbcl243
  (package
   (inherit sbcl)
   (name "sbcl-243")
   (version "2.4.3")
   (source
    (origin
     (method url-fetch)
     (uri (string-append "mirror://sourceforge/sbcl/sbcl/" version "/sbcl-"
                         version "-source.tar.bz2"))
     (sha256
      (base32 "1wn3nwq3qn33r6pxr57kwjhdx4w80f51akrx9b3x6amqjbgsmjc9"))
     (modules '((guix build utils)))
     (snippet '(begin
                 ;; Don't force ARMv5.
                 (substitute* "src/runtime/Config.arm-linux"
                              (("-march=armv5t") ""))))))))
