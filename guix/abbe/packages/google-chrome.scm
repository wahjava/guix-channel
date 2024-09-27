(define-module (abbe packages google-chrome)
               #:use-module ((nongnu packages chrome) #:select (make-google-chrome)))

(define-public google-chrome-latest
               (make-google-chrome "stable" "129.0.6668.70"
                                   "0nzjzdg1g7myvj20va5h25ax8128p99j24lmkglns65igjy9zvh5"))
