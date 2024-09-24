;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2024 Ashish SHUKLA <ashish.is@lostca.se>

(define-module (abbe packages signal)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (nongnu packages messaging))

(define-public signal-desktop-wayland
  (package/inherit signal-desktop
    (name "signal-desktop-wayland")
    (arguments
      (substitute-keyword-arguments
        (package-arguments signal-desktop)
        ((#:phases phases)
         #~(modify-phases #$phases
           (add-after 'setup-cwd 'waylandify-signal-desktop
             (lambda _
               (substitute* "share/applications/signal-desktop.desktop"
                 (("Exec[[:space:]]*=[[:space:]]*([^[:space:]]*)([[:space:]].+$)" _ signal-exe signal-args)
                  (string-append "Exec=" signal-exe
                                 " --enable-features=UseOzonePlatform"
                                 " --ozone-platform=wayland"
                                 signal-args)))))))))))
