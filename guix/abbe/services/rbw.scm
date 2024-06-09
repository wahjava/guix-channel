(define-module (abbe services rbw)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (gnu home services)
  #:use-module (gnu home services xdg)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu packages gnupg)
  #:use-module (json)
  #:use-module (ice-9 match)
  #:export  (home-rbw-configuration
	     home-rbw-configuration?
	     home-rbw-configuration-pinentry-program
	     home-rbw-configuration-email
	     home-rbw-configuration-base-url
	     home-rbw-configuration-identity-url
	     home-rbw-configuration-notifications-url
	     home-rbw-configuration-client-cert-path
	     home-rbw-configuration-sync-interval
	     home-rbw-configuration-lock-timeout

	     home-rbw-service-type))

(define-maybe string)

(define-configuration/no-serialization home-rbw-configuration
    (pinentry-program
      (file-like (file-append pinentry "/bin/pinentry-curses"))
      "Pinentry program to use.")
    (email
      (string "")
      "Email address of the user")
    (base-url
      maybe-string
      "Base URL")
    (identity-url
      maybe-string
      "Identity URL")
    (notifications-url
      maybe-string
      "Notifications URL")
    (client-cert-path
      maybe-string
      "Client certificate path")
    (sync-interval
      (integer 3600)
      "Synchronization interval")
    (lock-timeout
      (integer 3600)
      "Lock timeout"))

(define (home-rbw-config-files config)
  (define (rbw-config config)
    (match-record config <home-rbw-configuration>
		  (pinentry-program email base-url identity-url notifications-url
				    client-cert-path sync-interval lock-timeout)
		  `((pinentry-program . ,pinentry-program)
		    (email . ,email)
		    (base_url . ,base-url)
		    (identity_url . ,identity-url)
		    (notifications_url . ,notifications-url)
		    (client_cert_path . ,client-cert-path)
		    (sync_interval . ,sync-interval)
		    (lock_timeout . ,lock-timeout))))
  (define (home-rbw-configuration-file config)
    (let ((xformed-config (rbw-config config)))
      (computed-file "rbw-config.json"
		     #~(call-with-output-file #$output
					      (lambda (port)
						(display #$(scm->json-string xformed-config) port))))))
  `(("rbw/config.json" ,(home-rbw-configuration-file config))))

(define home-rbw-service-type
  (service-type
    (name 'home-rbw)
    (extensions
      (list (service-extension home-xdg-configuration-files-service-type
			       home-rbw-config-files)))
    (default-value (home-rbw-configuration))
    (description
      "Configure rbw")))
