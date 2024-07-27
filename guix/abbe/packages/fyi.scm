(define-module (abbe packages fyi)
  #:use-module (guix packages)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages pkg-config)
  #:use-module (guix git-download)
  #:use-module (guix build-system meson)
  #:use-module ((guix licenses) #:prefix license:))

(define-public fyi
  (package
    (name "fyi")
    (version "1.0.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://codeberg.org/dnkl/fyi")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256 (base32 "19l1w30wsm3gac0s1f81jxgmibi3qgx8f73hi047ykbxiwdccvqz"))))
    (build-system meson-build-system)
    (inputs (list dbus))
    (native-inputs (list pkg-config))
    (home-page "https://codeberg.org/dnkl/fyi")
    (synopsis "A lightweight alternative to notify-send")
    (description "FYI (for your information) is a command line utility to send
desktop notifications to the user via a notification daemon implementing XDG desktop
notifications.")
    (license license:expat)))
