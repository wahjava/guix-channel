(define-module (abbe services rosenpass)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (guix packages)
  #:use-module (guix modules)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services configuration)
  #:use-module (abbe packages rosenpass)
  #:export ( rosenpass-peer
             rosenpass-peer?
             rosenpass-peer-id
             rosenpass-peer-device
             rosenpass-peer-public-key
             rosenpass-peer-key-out
             rosenpass-peer-endpoint
             rosenpass-peer-device
             rosenpass-peer-extra-params
             rosenpass-peer->string

             rosenpass-configuration
             rosenpass-configuration?
             rosenpass-configuration-package
             rosenpass-configuration-peers
             rosenpass-configuration-listen
             rosenpass-configuration-public-key
             rosenpass-configuration-private-key
             rosenpass-configuration-verbosity
             rosenpass-configuration->string

             rosenpass-service-type))

(define %config-root
    "/etc/rosenpass")

(define %state-root
  "/var/lib/rosenpass")

(define-record-type* <rosenpass-peer>
  rosenpass-peer make-rosenpass-peer
  rosenpass-peer?
  (id rosenpass-peer-id)
  (public-key rosenpass-peer-public-key)
  (key-out rosenpass-peer-key-out)
  (endpoint rosenpass-peer-endpoint)
  (device rosenpass-peer-device)
  (extra-params rosenpass-peer-extra-params
    (default '())))

(define-record-type* <rosenpass-configuration>
  rosenpass-configuration make-rosenpass-configuration
  rosenpass-configuration?
  (package rosenpass-configuration-package
           (default rosenpass))
  (listen rosenpass-configuration-listen
             (default "0.0.0.0:54471"))
  (private-key rosenpass-configuration-private-key
               (default (string-append %config-root "/private")))
  (public-key rosenpass-configuration-public-key
              (default (string-append %config-root "/public")))
  (verbosity rosenpass-configuration-verbosity
             (default "Verbose"))
  (peers rosenpass-configuration-peers
         (default (list))))

(define (rosenpass-peer-block peer)
  (let ((public-key (rosenpass-peer-public-key peer)))
    (list "\n[[peers]]\n"
          (format #f "device = ~s~%"
                  (rosenpass-peer-device peer))
          "extra_params = []\n"
          "public_key = \""
          public-key
          "\"\n"
          (format #f "peer = ~s~%"
                  (rosenpass-peer-id peer))
          (format #f "key_out = ~s~%"
                  (string-append %state-root "/"
                                 (rosenpass-peer-key-out peer) ".key"))
          (format #f "endpoint = ~s~%"
                  (rosenpass-peer-endpoint peer)))))

(define (rosenpass-configuration-header config)
  (format #f
          (string-join
           (list "public_key = ~s"
                 "secret_key = ~s"
                 "verbosity = ~s"
                 "listen = [ ~s ]"
                 "")
           "\n")
          (rosenpass-configuration-public-key config)
          (rosenpass-configuration-private-key config)
          (rosenpass-configuration-verbosity config)
          (rosenpass-configuration-listen config)))

(define (rosenpass-activation config)
  "Creates rosenpass configuration and the keys"
  (with-imported-modules (source-module-closure '((guix gexp)
                                                  (guix utils)
                                                  (guix build utils)))
    #~(begin
        (use-modules (guix gexp))
        (let ((config-file (string-append #$%config-root "/" "config")))
          (unless (file-exists? config-file)
            (mkdir-p (dirname config-file)))

          (when (file-exists? config-file)
            (delete-file config-file))
          (call-with-output-file config-file
            (lambda (out)
              (display
               (string-append
                #$(rosenpass-configuration-header config)
                #$@(apply append
                          (map rosenpass-peer-block
                               (rosenpass-configuration-peers config)))
                "\n\n")
               out)))
          
          (unless (and (file-exists? #$(rosenpass-configuration-private-key config))
                       (file-exists? #$(rosenpass-configuration-public-key config)))
            (invoke #$(file-append (rosenpass-configuration-package config)
                                   "/bin/rosenpass") "gen-keys" config-file))
          (unless (directory-exists? #$%state-root)
            (mkdir-p #$%state-root))))))

(define (rosenpass-shepherd-service config)
  "Return a <shepherd-service> for rosenpass with CONFIG"
  (list
   (shepherd-service
    (provision '(rosenpass))
    (requirement '(wireguard-wg0))
    (start #~(make-forkexec-constructor
              (list #$(file-append rosenpass "/bin/rosenpass")
                    "exchange-config"
                    (string-append #$%config-root "/config"))
              #:log-file "/var/log/rosenpass.log"
              #:environment-variables
              (list (string-append "PATH="
                          (string-join
                           (filter directory-exists?
                                   (apply append (map (lambda (h) (list (string-append h "/sbin")
                                                                   (string-append h "/bin")))
                                                      (cons #$rosenpass
                                                            (map cadr
                                                                 (quote #$(package-propagated-inputs rosenpass))))))) ":")))))
    (stop #~(make-kill-destructor)))))

(define rosenpass-service-type
  (service-type
   (name 'rosenpass)
   (extensions
    (list (service-extension activation-service-type
                             rosenpass-activation)
          (service-extension shepherd-root-service-type
                             rosenpass-shepherd-service)))
   (default-value (rosenpass-configuration))
   (description "Launch rosenpass")))
