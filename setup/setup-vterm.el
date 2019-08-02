(use-package vterm
  ;; Use the package installed using nix
  :straight nil
  :config
  (defun akirak/vterm-exit (&optional buf)
    (bury-buffer (or buf (current-buffer))))
  (defun akirak/run-interactive-shell-command (command &optional name)
    (interactive "s")
    (let ((buffer (generate-new-buffer (or name "*vterm*"))))
      (with-current-buffer buffer
        (let ((vterm-shell command))
          (vterm-mode))
        (pop-to-buffer buffer))))
  (add-hook 'vterm-exit-functions #'akirak/vterm-exit))

(use-package vterm-toggle
  :straight (vterm-toggle :host github :repo "jixiuf/vterm-toggle")
  :custom
  ;; vterm-toggle uses pop-to-buffer to display the buffer, but it
  ;; deletes all the other windows by default.
  ;; To disable the behaviour, you have to set this variable to nil.
  (vterm-toggle-fullscreen-p nil))

(provide 'setup-vterm)