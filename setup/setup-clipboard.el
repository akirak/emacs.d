;; If WAYLAND_DISPLAY environment variable is defined, the machine
;; is considered to be running on Chrome OS, because I am currently
;; using Xorg in non Chrome OS Linux environments.
;; This can be changed in the future.
(when (and (getenv "WAYLAND_DISPLAY")
           (executable-find "wl-paste"))
  (advice-add 'interprogram-paste-function :override #'akirak/wayland-paste))

(defun akirak/wayland-paste ()
  (with-timeout 1
    (ignore-errors
      (let ((s (shell-command-to-string "wl-paste")))
        (unless (string-empty-p s)
          (string-trim-right s))))))

(use-package clipsave
  :straight (clipurl :host github :repo "akirak/clipurl.el")
  :config
  (clipsave-mode 1)
  :custom
  (clipsave-timer-interval 1))

(provide 'setup-clipboard)
