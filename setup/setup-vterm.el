(use-package vterm
  ;; Use the package installed using nix
  :straight (vterm :type built-in)
  :if (akirak/library-exists-p "vterm" 'verbose)
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
  (defun akirak/vterm-exit (&optional buf event)
    (message "%s: %s" (buffer-name) (or event "finished with unknown status"))
    (when (equal "finished\n" event)
      (quit-window nil (get-buffer-window buf))))
  (defun akirak/run-interactive-shell-command (command &optional name)
    (interactive "s")
    (let ((buffer (generate-new-buffer (or name "*vterm*"))))
      (with-current-buffer buffer
        (let ((vterm-shell command))
          (vterm-mode))
        (pop-to-buffer buffer))))
  (add-hook 'vterm-exit-functions #'akirak/vterm-exit))

(use-package vterm-toggle
  :if (featurep 'vterm)
  :straight (vterm-toggle :host github :repo "jixiuf/vterm-toggle")
  :custom
  ;; vterm-toggle uses pop-to-buffer to display the buffer, but it
  ;; deletes all the other windows by default.
  ;; To disable the behaviour, you have to set this variable to nil.
  (vterm-toggle-fullscreen-p nil))

(provide 'setup-vterm)
