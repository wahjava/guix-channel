(define-module (abbe packages lisp)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (gnu packages lisp)
  #:use-module (guix build utils))

(define-public sbcl-optim
  (package
   (inherit sbcl)
   (name "sbcl-optimized")
   (version "2.4.9")
   (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/sbcl/sbcl/" version "/sbcl-"
                           version "-source.tar.bz2"))
       (sha256
        (base32 "1qh56rgwg3np2cr26kgrrsagrnawkcnzb8bw9p33v56nqpmy8w4r"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; Don't force ARMv5.
           (substitute* "src/runtime/Config.arm-linux"
             (("-march=armv5t") ""))))))
   (native-inputs
    (modify-inputs (package-native-inputs sbcl)
                   (delete "clisp")
                   (prepend sbcl)))
   (arguments
    (substitute-keyword-arguments
     (package-arguments sbcl)
     ((#:phases phases)
      #~(modify-phases #$phases
                       (replace 'build
                                (lambda* (#:key outputs #:allow-other-keys)
                                         (setenv "CC" "gcc")
                                         (invoke "sh" "make.sh" "sbcl"
                                                 (string-append "--prefix=" (assoc-ref outputs "out"))
                                                 "--dynamic-space-size=10240"
                                                 "--with-sb-core-compression"
                                                 "--with-sb-xref-for-internals"
                                                 "--with-sb-simd")))))))))

;; Local Variables:
;; mode: scheme
;; tab-width: 2
;; indent-tabs-mode: nil
;; End:
