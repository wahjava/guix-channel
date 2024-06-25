;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2024 Ashish SHUKLA <ashish.is@lostca.se>

(define-module (abbe packages slack-desktop)
  #:use-module (gnu packages video)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages base)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (nonguix build-system chromium-binary)
  #:use-module (nonguix build-system binary)
  #:use-module (nonguix licenses)
  #:use-module (ice-9 string-fun))

(define-public slack-desktop
  (let ((binary-deps
         '("ffmpeg" "dbus" ("nss" "/lib/nss") "cups" "libxcomposite" "gcc" "at-spi2-core"
           "libxcb" "cairo" "nspr" "glibc" "libxfixes" "cups" "libx11" "glib"
           "libxdamage" "libdrm" "libxrandr" "alsa-lib" "libxrandr" "expat" "pango"
           ("out" "/lib/slack") "mesa" "libxkbcommon" "gtk+" "libxext")))
    (package
     (name "slack-desktop")
     (version "4.38.125")
     (source (origin
              (method url-fetch)
              (uri
               (string-append
                "https://downloads.slack-edge.com/desktop-releases/linux/x64/"
                version "/" name "-" version "-amd64.deb"))
              (sha256
               (base32 "1mzk7qqkgdj9pyq4nhm4dfj8ik3ydj0ldn3bk01b6k8jkxfqb5q4"))))
     (build-system chromium-binary-build-system)
     (arguments
      (list
       #:patchelf-plan
       `'(("usr/bin/slack"  ,binary-deps)
          ("usr/lib/slack/slack" ,binary-deps)
          ("usr/lib/slack/libEGL.so" ("gcc"))
          ("usr/lib/slack/chrome_crashpad_handler" ("gcc" "glibc"))
          ("usr/lib/slack/libvk_swiftshader.so" ("gcc" "glibc"))
          ("usr/lib/slack/libvulkan.so.1" ("gcc"))
          ("usr/lib/slack/libffmpeg.so" ("gcc"))
          ("usr/lib/slack/libGLESv2.so" ("libx11" "libxext" "libxext" "libdrm" "gcc" "libxcb" "glibc")))
       #:validate-runpath? #f
       #:install-plan
       `'(("usr/bin/slack" "bin/")
          ("usr/lib/" "lib/")
          ("usr/share/" "share/"))
       #:phases
       #~ (modify-phases (@ (nonguix build chromium-binary-build-system) %standard-phases)
	    (add-after 'install 'install-desktop-file
	      (lambda* (#:key inputs #:allow-other-keys)
		(let* ((applications-dir (string-append #$output "/share/applications"))
		       (ozone-features
			(list "--enable-features=WaylandWindowDecorations" "--ozone-platform-hint=auto"))
		       (slack-executable (string-join (cons* (string-append #$output "/bin/slack")
							     ozone-features)
						      " ")))
		  (make-desktop-entry-file
		   (string-append applications-dir "/slack.desktop")
		   #:name "Slack"
		   #:comment "Slack Desktop"
		   #:generic-name "Slack Client for Linux"
		   #:exec (string-append slack-executable " %U")
		   #:try-exec slack-executable
		   #:icon (string-append #$output "/share/pixmaps/slack.png")
		   #:startup-w-m-class "Slack"
		   #:categories "GNOME;GTK;Network;InstantMessaging;"
		   #:mime-type "x-scheme-handler/slack")))))))

      (inputs (list ffmpeg dbus nss cups libxcomposite libx11 libxext libxrandr
		    libxfixes libxdamage libxcb glibc at-spi2-core cairo nspr
                    libxkbcommon glib mesa gtk+ (list gcc "lib") libdrm alsa-lib expat))
      
     (synopsis  "Slack communication system")
     (supported-systems '("x86_64-linux"))
     (description "Slack communication system")
     (home-page "https://slack.com/")
     (license (nonfree "https://www.google.com/intl/en/chrome/terms/")))))
