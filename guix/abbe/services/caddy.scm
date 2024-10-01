(define-module (abbe services caddy)
         #:use-module (abbe packages caddy)
         #:use-module (gnu services)
         #:use-module (gnu services shepherd)
         #:use-module (gnu services configuration)
         #:use-module (guix records)
         #:use-module (guix gexp)
         #:use-module (ice-9 match)
         #:use-module (gnu packages base)
         #:use-module (gnu packages linux)
         #:use-module (guix build utils)
         #:export (caddy-service-type))

(define %caddy-pidfile
  "/run/caddy.pid")

(define %caddy-home
  "/var/lib/caddy")

(define %caddy-log
  "/var/log/caddy")

(define (caddy-activation _)
  (begin
    (mkdir-p %caddy-home)
    (mkdir-p %caddy-log)))

(define (caddy-shepherd-service config)
  "Return a <shepherd-service> for caddy with CONFIG file"
  (let ((environment #~(list "XDG_DATA_HOME=/var/lib"
                             "XDG_CONFIG_HOME=/var/lib")))
    (list
     (shepherd-service
      (provision '(caddy))
      (requirement '()) ;; services this depends on
      (start #~(make-forkexec-constructor
                (list #$(file-append caddy "/bin/caddy")
                      "start"
                      "--config"
                      #$config
                      "--adapter"
                      "caddyfile"
                      "--pidfile"
                      #$%caddy-pidfile)
                #:pid-file #$%caddy-pidfile
                #:environment-variables #$environment))
      (stop #~(make-kill-destructor))))))

(define caddy-service-type
  (service-type
    (name 'caddy)
    (extensions
     (list
      (service-extension activation-service-type
                         caddy-activation)
      (service-extension shepherd-root-service-type
                         caddy-shepherd-service)))
    (default-value "/dev/null")
    (description "Launch caddy.")))
