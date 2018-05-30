(autoload 'akirak/emacs-lisp-hydra "emacs-lisp-hydra"
  "Hydra for Emacs Lisp." t)
(autoload 'akirak/org-hydra "org-hydra"
  "Hydra for Org." t)

(defcustom akirak/hydra-launcher-major-mode-command-alist
  '((emacs-lisp-mode . akirak/emacs-lisp-hydra)
    (org-mode . akirak/org-hydra))
  "Alist of major modes and commands for hydra-launcher.")

;;;###autoload
(defun akirak/hydra-launcher-for-major-mode ()
  (interactive)
  (cl-destructuring-bind command
      (alist-get major-mode akirak/hydra-launcher-major-mode-command-alist)
    (if command
        (funcall command)
      (cond
       ((derived-mode-p 'org-mode) (akirak/org-hydra))
       ((message "No hydra is available in this major mode"))))))

(provide 'hydra-launcher)
