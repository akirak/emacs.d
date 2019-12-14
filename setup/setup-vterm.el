(use-package vterm
  ;; Use the package installed using nix
  :straight (vterm :type built-in)
  :if (akirak/library-exists-p "vterm" 'verbose)
  :general
  (:keymaps 'vterm-mode-map
            "<S-prior>" #'scroll-down-command
            "<S-next>" #'scroll-up-command
            "C-c C-t" #'vterm-copy-mode
            "C-c C-c" #'vterm-send-ctrl-c)
  :config/el-patch
  ;; fzy doesn't seem to work with the default implementation.
  (el-patch-defun vterm-send-return ()
    (interactive)
    (el-patch-swap (vterm-send-key "<return>")
                   (process-send-string vterm--process "\C-m")))
  :config
  (general-setq vterm-keymap-exceptions
                (cl-merge 'list vterm-keymap-exceptions
                          '("M-r"
                            "M-g"
                            "M-s")
                          #'string-equal))
  (defun akirak/vterm-quit-window (&optional buf)
    (if-let ((window (get-buffer-window buf)))
        (quit-window nil window)
      (bury-buffer buf)))
  (defun akirak/run-interactive-shell-command (command &optional name)
    (interactive "s")
    (let ((buffer (generate-new-buffer (or name (format "*%s*" command)))))
      (with-current-buffer buffer
        (let ((vterm-shell command))
          (vterm-mode))
        (pop-to-buffer buffer)
        (remove-hook 'vterm-exit-functions #'akirak/vterm-quit-window :local))))
  (add-hook 'vterm-exit-functions #'akirak/vterm-quit-window))

(use-package vterm-toggle
  :if (featurep 'vterm)
  :custom
  ;; vterm-toggle uses pop-to-buffer to display the buffer, but it
  ;; deletes all the other windows by default.
  ;; To disable the behaviour, you have to set this variable to nil.
  (vterm-toggle-fullscreen-p nil))

(provide 'setup-vterm)
