(define-module (abbe packages google-chrome)
               #:use-module ((nongnu packages chrome) #:select (make-google-chrome)))

(define-public google-chrome-latest
               (make-google-chrome "stable" "127.0.6533.99"
                                   "0n6dxjzvbc5p4nhpa9f68isgfvs42d59j4argkiga2xqm14qphd4"))
