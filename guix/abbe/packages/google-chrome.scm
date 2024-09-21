(define-module (abbe packages google-chrome)
               #:use-module ((nongnu packages chrome) #:select (make-google-chrome)))

(define-public google-chrome-latest
               (make-google-chrome "stable" "129.0.6668.58"
                                   "1jlw2mc32nbhga2irgla055m1qfy7q7b7gl42dzglwk2jz10cmll"))
