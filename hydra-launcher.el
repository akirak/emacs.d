(autoload 'akirak/emacs-lisp-hydra "emacs-lisp-hydra")

(defcustom akirak/hydra-launcher-major-mode-command-alist
  '((emacs-lisp-mode . akirak/emacs-lisp-hydra))
  "Alist of major modes and commands for hydra-launcher.")

;;;###autoload
(defun akirak/hydra-launcher-for-major-mode ()
  (interactive)
  (cl-destructuring-bind command
      (alist-get major-mode akirak/hydra-launcher-major-mode-command-alist)
    (if command
        (funcall command)
      (message "No hydra is available in this major mode"))))

(provide 'hydra-launcher)
