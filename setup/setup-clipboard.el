(defcustom akirak/clipboard-paste-command
  (cond
   ((and (eq akirak/window-system 'wayland)
         (executable-find "wl-paste"))
    "wl-paste")
   ((and (eq akirak/window-system 'x)
         (executable-find "xclip"))
    "xclip -o"))
  "Command used to get the content of the system clipboard."
  :type 'string
  :set (lambda (sym val)
         (set sym val)
         (if val
             (advice-add 'interprogram-paste-function
                         :override #'akirak/interprogram-paste)
           (advice-remove 'interprogram-paste-function
                          #'akirak/interprogram-paste))))

(defun akirak/interprogram-paste ()
  (with-timeout 1
    (ignore-errors
      (let ((s (shell-command-to-string akirak/clipboard-paste-command)))
        (unless (string-empty-p s)
          (string-trim-right s))))))

(use-package clipsave
  :straight (clipurl :host github :repo "akirak/clipurl.el")
  :config
  (clipsave-mode 1)
  :custom
  (clipsave-timer-interval 1))

(provide 'setup-clipboard)
