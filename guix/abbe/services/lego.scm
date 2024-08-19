(define-module (abbe services lego)
  #:use-module (abbe packages lego)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services mcron)
  #:use-module (gnu services configuration)
  #:use-module (guix records)
  #:use-module (guix modules)
  #:use-module (guix gexp)
  #:use-module (ice-9 match)
  #:use-module (gnu packages base)
  #:use-module (gnu packages linux)
  #:use-module (guix build utils)
  #:use-module ((srfi srfi-1) #:select (fold-right fold))
  #:export (lego-service-type

            make-lego-configuration
            make-lego-domain-configuration
            
            lego-configuration
            lego-configuration?
            lego-configuration-email
            lego-configuration-domains
            lego-configuration-acme-endpoint

            lego-domain-configuration
            lego-domain-configuration?
            lego-domain-configuration-domain-name
            lego-domain-configuration-sans
            lego-domain-configuration-algorithm
            lego-domain-configuration-env-vars
            lego-domain-configuration-challenge-type
            lego-domain-configuration-acme-endpoint
            lego-domain-configuration-email))

(define %lego-home
  "/var/lib/lego")

(define-record-type* <lego-domain-configuration>
  lego-domain-configuration make-lego-domain-configuration
  lego-domain-configuration?
  (domain-name lego-domain-configuration-domain-name
               (default #f))
  (email lego-domain-configuration-email
         set-lego-domain-configuration-email
         (default #f))
  (acme-endpoint lego-domain-configuration-acme-endpoint
                 set-lego-domain-configuration-acme-endpoint
                 (default #f))
  (sans lego-domain-configuration-sans
        (default '()))
  (algorithm lego-domain-configuration-algorithm
             (default "ec384"))
  (env-vars lego-domain-configuration-env-vars
            (default '()))
  (challenge-type lego-domain-configuration-challenge-type
                  (default "dns"))
  (challenge-provider lego-domain-configuration-challenge-provider
                      (default #f)))

(define-record-type* <lego-configuration>
  lego-configuration make-lego-configuration
  lego-configuration?
  (email lego-configuration-email
         (default #f))
  (domains lego-configuration-domains
           (default (list)))
  (acme-endpoint lego-configuration-acme-endpoint
                 (default "https://acme-staging-v02.api.letsencrypt.org/directory")))

(define (lego-domain-configuration->mcron-job)
  (match-lambda
    (($ <lego-domain-configuration> domain-name email acme-endpoint sans
                                    algorithm env-vars challenge-type
                                    challenge-provider)
     (program-file
      (string-append  "lego-renewal-" domain-name)
      (with-imported-modules '((guix build utils))
        #~(begin
            (use-modules (guix build utils))
            (let ((domains (list  #$@(fold-right (lambda (domain domains)
                                                   (cons* "--domains" domain domains))
                                                 (list)
                                                 (cons domain-name sans))))
                  (method (list #$(string-append "--" challenge-type)
                                #$@(if (string? challenge-provider)
                                       (list challenge-provider)
                                       (list)))))
              (for-each (lambda (env) (setenv (car env) (cdr env)))
                        (quote #$env-vars))
              (apply invoke (cons* (string-append #$lego "/bin/lego")
                                   "--accept-tos"
                                   "--path"
                                   #$%lego-home
                                   "--key-type"
                                   #$algorithm
                                   "--email"
                                   #$email
                                   "--server"
                                   #$acme-endpoint
                                   (append method domains '("run")))))))))))

(define (lego-cron lego-config)
  (let ((patch-email
         (lambda (config)
           (if (string? (lego-domain-configuration-email config))
               config
               (lego-domain-configuration
                (inherit config)
                (email (lego-configuration-email lego-config))))))
        (patch-acme-endpoint
         (lambda (config)
           (if (string? (lego-domain-configuration-acme-endpoint config))
               config
               (lego-domain-configuration
                (inherit config)
                (acme-endpoint  (lego-configuration-acme-endpoint lego-config)))))))
    (map (compose (lambda (config)
                    #~(job '(next-day)
                           #$((lego-domain-configuration->mcron-job) config)))
                  patch-email
                  patch-acme-endpoint)
         (lego-configuration-domains lego-config))))

(define (lego-activation _)
  (mkdir-p %lego-home))

(define lego-service-type
  (service-type
    (name 'lego)
    (extensions
     (list
      (service-extension activation-service-type
                         lego-activation)
      (service-extension mcron-service-type
                         lego-cron)))
    (default-value (lego-configuration))
    (description "Launch lego.")))
