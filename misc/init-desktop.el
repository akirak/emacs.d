(defun akirak/maybe-save-desktop-and-kill-emacs (arg)
  "Kill the desktop.

With prefix ARG, save all the buffers silently and kill Emacs without
confirmation."
  (interactive "P")
  (unless arg
    (when (and (server-running-p)
               (y-or-n-p "Save all the buffers as well as the desktop?"))
      (save-some-buffers t)
      (desktop-save-in-desktop-dir)))
  (if arg
      (let ((confirm-kill-emacs nil))
        (save-buffers-kill-emacs t))
    (save-buffers-kill-emacs)))

(global-set-key (kbd "C-x C-c") #'akirak/maybe-save-desktop-and-kill-emacs)

(defun akirak/startup-read-desktop ()
  (when (and (server-running-p)
             (featurep 'exwm))
    (desktop-read)))

(add-hook 'after-init-hook #'akirak/startup-read-desktop)

(setq-default desktop-restore-frames nil)

(provide 'init-desktop)
