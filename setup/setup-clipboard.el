;; If WAYLAND_DISPLAY environment variable is defined, the machine
;; is considered to be running on Chrome OS, because I am currently
;; using Xorg in non Chrome OS Linux environments.
;; This can be changed in the future.
(when (and (getenv "WAYLAND_DISPLAY")
           (executable-find "wl-paste"))
  (advice-add 'interprogram-paste-function
              :filter-return #'akirak/ad-after-interprogram-paste-function))

(defun akirak/ad-after-interprogram-paste-function (r)
  ;; If the result from the original function is nil, use wl-paste
  ;; to get the clipboard content.
  (or r (ignore-errors (akirak/paste-using-wl-paste))))

(defun akirak/paste-using-wl-paste ()
  (let ((s (shell-command-to-string "wl-paste")))
    (unless (string-empty-p s)
      (string-trim-right s))))

(use-package clipsave
  :straight (clipurl :host github :repo "akirak/clipurl.el")
  :init
  (clipsave-mode 1))

(provide 'setup-clipboard)
