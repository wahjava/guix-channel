(define-module (abbe services emacs)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (gnu packages emacs)
  #:use-module (gnu home services shepherd)
  #:use-module (guix gexp))

(define-public emacs-daemon
  (shepherd-service
   (provision '(emacs-daemon))
   (documentation "Run `emacs-daemon'")
   (modules '((shepherd support)))
   (start	#~(make-forkexec-constructor
		   (list
		    (string-append #$emacs-no-x "/bin/emacs")
		    "--fg-daemon")
		   #:log-file (string-append %user-log-dir "/emacs-daemon.log")))
   (stop #~(make-kill-destructor))
   (one-shot? #f)
   (respawn? #t)))
