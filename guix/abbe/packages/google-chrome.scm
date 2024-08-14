(define-module (abbe packages google-chrome)
               #:use-module ((nongnu packages chrome) #:select (make-google-chrome)))

(define-public google-chrome-latest
               (make-google-chrome "stable" "127.0.6533.119"
                                   "0pk6x9h0bzg231zivcdr9gcw9gifg0skjp9d995mff1dn08frnlk"))
