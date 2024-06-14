(define-module (abbe packages firefox)
  #:use-module (guix packages)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages base)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages xorg)
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module (nonguix build-system binary)
  #:use-module ((guix licenses) :prefix license:)
  #:use-module (ice-9 match))

(define-public (make-firefox name version hash)
  (package
   (name name)
   (version version)
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "http://archive.mozilla.org/pub/firefox/releases/" version 
                  "/linux-x86_64/en-US/firefox-" version ".tar.bz2"))
            (sha256 (base32 hash))))
   (build-system binary-build-system)
   (arguments
    `(
      #:patchelf-plan
      `(("firefox" ("glibc" "gcc"))
        ("pingsender" ("glibc" "gcc"))
        ("glxtest" ("glibc" "libxcb" "libx11" "libxext" "cairo" "pango" "gtk+" "libxrandr" "libxdamage" "gcc" "glib" 
                    "libxi" "libxfixes" "gdk-pixbuf" "libxcomposite" "at-spi2-core" "libxcursor"))
        ("updater" ("dbus" "cairo" "glib" "gtk+" "libx11" "libxfixes" "libxcursor" "freetype" "libxrandr" "libxext" "at-spi2-core" "out"))
        ("libxul.so" ("dbus" "cairo" "glib" "gtk+" "libx11" "libxfixes" "libxcursor" "freetype" "libxrandr" "libxext" "at-spi2-core" "out"))
        ("libmozgtk.so" ("gtk+"))
        ("libsmime3.so" ( "out"))
        ("libmozsandbox.so" ( "out" "gcc"))
        ("crashreporter" ( "pango" "gtk+" "glib" "gcc" "gdk-pixbuf" "glibc"))
        ("liblgpllibs.so" ( "gcc" "glibc"))
        ("libfreeblpriv3.so" ("out"))
        ("firefox-bin" ("gcc" "glibc"))
        ("libipcclientcerts.so" ( "gcc" "glibc"))
        ("libxul.so" ( "libxcursor" "out" "alsa-lib" "dbus" "gdk-pixbuf" "freetype" "at-spi2-core" "libxcomposite" "libxdamage" "cairo" "libxrandr"
                       "fontconfig-minimal" "glib" "pango" "gtk+" "libxcb" "libxi" "gcc" "gdk-pixbuf" "libxfixes" "libx11" "libxext" "libxrender" "glibc"))
        ("libmozavutil.so" ( "out"))
        ("updater" ( "pango" "gtk+" "gcc" "gdk-pixbuf" "cairo" "out" "glib" "at-spi2-core"))
        ("libsoftokn3.so" ( "out" ))
        ("vaapitest" ( "gdk-pixbuf" "libxcb" "libx11" "libxext" "libxrandr" "libxcomposite" "libxcursor" "libxdamage" "libxfixes" "libxi" "pango" "at-spi2-core" "cairo" "gtk+" "glib" "gcc"))
        ("libssl3.so" ( "out"))
        ("libnss3.so" ( "out"))
        ("libnssutil3.so" ( "out"))
        ("libplc4.so" ( "out"))
        ("libplds4.so" ( "out"))
        ("libgkcodecs.so" ( "gcc" ))
        ("libnssckbi.so" ( "gcc" "glibc" ))
        ("libmozavcodec.so" ( "out" "gcc"))
        ("minidump-analyzer" ( "gcc" "glibc" ))
        ("gmp-clearkey/0.1/libclearkey.so" ( "out" "gcc")))
      #:install-plan
      `(("firefox" "bin/")
	("firefox-bin" "bin/")
        ("./" "lib/"))))
   (inputs (list glibc `(,gcc "lib") gtk+ cairo pango libxcb libx11 libxext at-spi2-core
                 glib dbus freetype libxrandr libxdamage libxfixes libxcursor alsa-lib
                 libxcomposite fontconfig libxi libxrender gdk-pixbuf))
   (synopsis "Firefox is a web browser.")
   (description
    "Browser")
   (home-page "https://firefox.com/")
   (license license:bsd-3)))

(define-public firefox-bin
  (make-firefox "firefox-bin" "127.0" "1s8ainl9dh7l13bvr5abirn0r7903frprjljsxl31js84czm46hd"))
