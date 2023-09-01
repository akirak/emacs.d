(defcustom akirak/clipboard-paste-command
  (cond
   ((and (eq akirak/window-system 'wayland)
         (executable-find "wl-paste"))
    "wl-paste"))
  "Command used to get the content of the system clipboard."
  :type 'string
  :set (lambda (sym val)
         (set sym val)
         (setq interprogram-paste-function
               (if val
                   #'akirak/interprogram-paste
                 #'gui-selection-value))))

(defun akirak/interprogram-paste ()
  (with-timeout (1 nil)
    (ignore-errors
      (let ((s (shell-command-to-string akirak/clipboard-paste-command)))
        (unless (string-empty-p s)
          (string-trim-right s))))))

(use-package clipsave
  :straight (clipurl :host github :repo "akirak/clipurl.el")
  :unless (or (akirak/running-on-crostini-p)
              (akirak/windows-subsystem-for-linux-p))
  :config
  (clipsave-mode 1)
  :custom
  (clipsave-timer-interval 1))

(provide 'setup-clipboard)
