(use-package gif-screencast
  :general
  (:keymaps 'gif-screencast-mode-map
            "<S-f8>" 'gif-screencast-toggle-pause
            "<f8>" 'gif-screencast-stop)
  ;; TODO: Install dependencies using nix
  ;; FIXME: This doesn't work on Chrome OS. It may be due to Waylaid.
  ;; Maybe I will use grim: https://github.com/emersion/grim
  ;; TODO: How can I take screnshots from a command line on Windows?
  ;; :ensure-system-package
  ;; ((scrot . scrot)
  ;;  (convert . imagemagick)
  ;;  (gifsicle . gifsicle))
  )

(provide 'setup-screencast)
