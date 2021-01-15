(use-package git-identity
  :after magit
  :straight (git-identity :host github :repo "akirak/git-identity.el")
  :general
  (:keymaps 'magit-status-mode-map :package 'magit
            "I" #'git-identity-info)

  :config
  (cl-defmacro akirak/git-identity-add (address &rest args)
    (declare (indent 1))
    (let ((cell (assoc address git-identity-list)))
      (if cell
          (setcdr cell args)
        (push (cons address args)
              git-identity-list))))

  :custom
  (git-identity-magit-mode t))

(provide 'setup-repos)
