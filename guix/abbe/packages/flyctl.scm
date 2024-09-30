(define-module (abbe packages flyctl)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (abbe build-system nix-go)
  #:use-module (abbe packages go)
  #:use-module ((guix licenses) #:prefix license:))

(define-public flyctl
  (package
   (name "flyctl")
   (version "0.3.12")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/superfly/flyctl")
                  (commit (string-append "v" version))))
            (sha256 (base32 "1fh9al65cj07wfr504vzqxr5zcv9qsqq32jr1pcvlmvmcw4vr9p6"))
            (modules '((guix build utils)))
            (snippet
             #~(begin
                 (substitute* "internal/config/config.go"
                   (("w.AutoUpdate = true")
                    "w.AutoUpdate = false"))))))
   (build-system nix-go-build-system)
   (arguments
    `(#:go ,go-122
      #:tags '("production")
      #:ldflags `("-X"
                  "github.com/superfly/flyctl/internal/buildinfo.buildDate=1970-01-01T00:00:00Z"
                  "-X"
                  "github.com/superfly/flyctl/internal/buildinfo.branchName=master"
                  "-X"
                  ,,(string-append "github.com/superfly/flyctl/internal/buildinfo.buildVersion=" version))
     #:vendor-hash "0g5ivrak5vj4kb2knc3ww4i5m1r1xbkgga5dz3gdbkrcc2mr2njz"))
   (synopsis "Command line tools for fly.io services")
   (description
    "flyctl is a command-line interface for fly.io")
   (home-page "https://fly.io")
   (license license:asl2.0)))
