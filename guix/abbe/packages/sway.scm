(define-module (abbe packages sway)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix modules)
  #:use-module (gnu packages build-tools)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages wm))

(define current-module-file
  (search-path %load-path
                   (module-name->file-name
                    (module-name (current-module)))))

(define current-module-directory
  (dirname (and=> current-module-file canonicalize-path)))

(define wayland-next
  (package/inherit wayland
    (version "1.23.0")
    (name "wayland")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://gitlab.freedesktop.org/" name
                                  "/" name "/-/releases/" version
                                  "/downloads/" name "-" version ".tar.xz"))
              (sha256 (base32 "1cjswi1d7hp6lhvcnrdrxry7qhjvdgrn5y3lb5mn4rry9mby3cq5"))))))

(define wayland-protocols-next-2
  (package/inherit wayland-protocols
    (version "1.36.0")
    (name "wayland-protocols")
    (inputs (list wayland-next))
    (native-inputs
      (modify-inputs (package-native-inputs wayland-protocols)
        (replace "wayland" wayland-next)))
    (source (origin
              (method url-fetch)
              (uri (string-append "https://gitlab.freedesktop.org/wayland/" 
                                  "/" name "/-/releases/" version
                                  "/downloads/" name "-" version ".tar.xz"))
              (sha256 (base32 "1cjswi1d7hp6lhvcnrdrxry7qhjvdgrn5y3lb5mn4rry9mby3cq5"))))))

(define libdrm-next
  (package/inherit libdrm
    (name "libdrm")
    (version "2.4.122")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://dri.freedesktop.org/libdrm/libdrm-"
                                  version ".tar.xz"))
              (sha256 (base32 "0lgjj9ign3cl27hzmqnxr7xwv21mm486pifc029wmzvxfydhgxfr"))))))

(define meson-next
  (package/inherit meson/newer
                   (version "1.5.1")
                   (source
                     (origin
                       (method url-fetch)
                       (uri (string-append "https://github.com/mesonbuild/meson/"
                                           "releases/download/" version "/meson-"
                                           version ".tar.gz"))
                       (sha256 (base32 "1vab706pw1q17vh9rbsmlirgiji3k6wljl735lxffp95vwx56zjn"))))))

(define libinput-minimal-next
  (package/inherit libinput-minimal
    (version "1.26.1")
    (arguments
      (cons* 
        #:tests? #f
        (substitute-keyword-arguments (package-arguments libinput-minimal)
          ((#:meson old-meson meson)
           meson-next))))
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://gitlab.freedesktop.org/libinput/libinput.git")
                     (commit version)))
              (sha256 (base32 "094k23i10jjmgdzhv5lj7xpc3fgkwi9afcihf0l2fjj71ym8l9fy"))))))

(define wlroots-git
  (package/inherit wlroots
    (name "wlroots")
    (version "0.19")
    (arguments
      (substitute-keyword-arguments (package-arguments wlroots)
        ((#:meson old-meson meson)
         meson-next)))
    (native-inputs (modify-inputs (package-native-inputs wlroots)
                      (replace "wayland-protocols" wayland-protocols-next)
                      (replace "wayland" wayland-next)))
    (propagated-inputs (modify-inputs (package-propagated-inputs wlroots)
                           (replace "wayland" wayland-next)
                           (replace "wayland-protocols" wayland-protocols-next)
                           (replace "libinput-minimal" libinput-minimal-next)
                           (append libdrm-next)))
    (source (origin (method git-fetch)
                    (uri (git-reference
                         (url "https://gitlab.freedesktop.org/wlroots/wlroots")
                         (commit "8730ca9661eaaab83954bca033745b65c60cf4f8")))
                    (file-name (git-file-name name version))
                    (sha256 (base32 "1sw5scb9gidijwh1fmxjzqcnv9638r29qdakk2fbdq3mhg8g8dqx"))))))

(define wlroots-git-with-tile
  (package-with-extra-patches wlroots-git
    (map (lambda (f) (local-file (string-append current-module-directory
                                                "/patches/" f)))
         (list "wlroots-tile-display.patch"))))

(define-public sway-with-tile
  (package/inherit sway
    (version "1.10-HEAD")
    (arguments
      (substitute-keyword-arguments (package-arguments sway)
        ((#:meson old-meson meson)
         meson-next)))
    (source (origin 
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/swaywm/sway")
                     (commit "951a22c2445f5c32b831bac0db86869627940402")))
              (file-name (git-file-name "sway" version))
              (sha256 (base32 "13x48441hn5xzqk040cvns3q02r5bl3k2qja74v5k91i2ybkpc7b"))))
    (native-inputs (modify-inputs (package-native-inputs sway)
                      (prepend wayland-next)
                      (prepend wlroots-git-with-tile)
                      (replace "wayland-protocols" wayland-protocols-next)))
    (inputs (modify-inputs (package-inputs sway)
              (replace "libinput-minimal" libinput-minimal-next)
              (replace "wlroots" wlroots-git-with-tile)
              (replace "wayland" wayland-next)))))
