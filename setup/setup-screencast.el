;; NOTE: This doesn't work on Chrome OS, since scrot doesn't work on
;; Wayland. There are some alternatives that support some compositors
;; on Wayland, but none of them seem to run on Chrome OS. Maybe I'll
;; try weston-screenshooter.
(use-package gif-screencast
  :general
  (:keymaps 'gif-screencast-mode-map
            "<S-f8>" 'gif-screencast-toggle-pause
            "<f8>" 'gif-screencast-stop)
  :ensure-system-package
  ((scrot . scrot)
   (convert . imagemagick)
   (gifsicle . gifsicle)))

(provide 'setup-screencast)
