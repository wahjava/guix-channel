(define-module (abbe packages google-chrome)
               #:use-module ((nongnu packages chrome) #:select (make-google-chrome)))

(define-public google-chrome-latest
               (make-google-chrome "stable" "129.0.6668.89"
                                   "0p0v5vx29r0rndp1rfkdrssr6m04z7bbfvp6cwbixxxnhnq97j7f"))
