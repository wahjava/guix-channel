(define-module (abbe services sshguard)
         #:use-module (abbe packages sshguard)
         #:use-module (gnu services)
         #:use-module (gnu services shepherd)
         #:use-module (gnu services configuration)
         #:use-module (guix records)
         #:use-module (guix gexp)
         #:use-module (ice-9 match)
         #:use-module (gnu packages base)
         #:use-module (gnu packages linux)
         #:export (sshguard-service-type sshguard-configuration))

(define-record-type* <sshguard-configuration>
         sshguard-configuration make-sshguard-configuration
         sshguard-configuration?
             (backend sshguard-configuration-backend
                (default 'nft))
         (whitelist-ips sshguard-configuration-whitelist-ips
          (default '()))
         (whitelist-file sshguard-configuration-whitelist-file
          (default #f))
         (blacklist-threshold sshguard-configuration-blacklist-threshold
          (default 10))
         (blacklist-file sshguard-configuration-blacklist-file
           (default #f))
         (detection-time sshguard-configuration-detection-time
           (default 1800))
         (block-time sshguard-configuration-block-time
           (default 120)))

(define (sshguard-activation config)
  "Run sshguard --cleanup"
  #~(begin
      (system* #$(file-append sshguard "/bin/sshguard") "--cleanup")))

(define (sshguard-shepherd-service config)
  "Return a <shepherd-service> for Tailscaled with CONFIG"
  (let ((whitelist (apply append (map (lambda (ip) (list  "-w" ip))
                                      (sshguard-configuration-whitelist-ips config))))
        (blacklist-threshold (list "-a" (number->string (sshguard-configuration-blacklist-threshold config))))
        (blacklist-file (let ((blacklist-file (sshguard-configuration-blacklist-file config)))
                          (if (string? blacklist-file)
                            (list "-b" blacklist-file)
                            (list))))
        (block-time (list "-p" (number->string (sshguard-configuration-block-time config))))
        (detection-time (list "-s" (number->string (sshguard-configuration-detection-time config))))
        (environment #~(list
                        (string-append "PATH=" ; iptables is required for sshguard to work
                                       (string-append #$nftables "/sbin")
                                       ":"
                                       (string-append #$nftables "/bin"))
                        (string-append "SSHGUARD_CONFIG_FILE="
                                       (local-file "sshguard.conf"
                                                   (string-join
                                                     (list (string-append "BACKEND=\"" #$sshguard "/libexec/sshg-fw-nft-sets\"")
                                                           (string-append "LOGREADER=\"LANG=C " #$coreutils "/bin/tail -F -n 100000 /var/log/secure\""))
                                                     "\n"))))))
      (list
       (shepherd-service
           (provision '(sshguard))
        (requirement '(networking)) ;; services this depends on
        (start #~(make-forkexec-constructor
                  (cons #$(file-append sshguard "/sbin/sshguard")
                        (append whitelist
                                blacklist-threshold
                                blacklist-file
                                block-time
                                detection-time
                                '("/var/log/secure")))
                  #:environment-variables #$environment))
        (stop #~(make-kill-destructor))))))

(define sshguard-service-type
  (service-type
    (name 'sshguard)
    (extensions
      (list (service-extension shepherd-root-service-type
             sshguard-shepherd-service)))
    (default-value (sshguard-configuration))
    (description "Launch sshguard.")))
