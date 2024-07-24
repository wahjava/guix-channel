(define-module (abbe packages sshguard)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages python-xyz)
  #:use-module ((guix licenses) #:prefix license:))

(define-public sshguard
  (package
    (name "sshguard")
    (version "2.4.3")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://bitbucket.org/sshguard/sshguard.git")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256 (base32 "1dkijr287zpwdz1bjdchxzmwf1sk6vzpkycz1skm25lkaba6nd9r"))))
    (build-system gnu-build-system)
    (native-inputs
      (list autoconf automake bison flex python-docutils))
    (arguments
      `(#:phases
        (modify-phases %standard-phases
          ;; patch sshguard to find configuration through
          ;; environment variable
          (add-after 'unpack 'patch-sshguard
            (lambda* _
              (substitute "src/sshguard.in"
                (list (cons "^config="
                            (lambda _
                              "config=${SSHGUARD_CONFIG_FILE:-@sysconfdir@/sshguard.conf}\n")))))))))

    (home-page "https://sshguard.net/")
    (synopsis "Daemon to blocks SSH brute-force attacks")
    (description "sshguard protects hosts from brute-force attacks against SSH and
other services.  It aggregates system logs and blocks repeat offenders using one of
several firewall backends.")
    (license license:isc)))
