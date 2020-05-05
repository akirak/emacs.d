(require 'my/dir/function)

(defconst akirak/helm-git-project-actions
  '(("Magit status" . magit-status)
    ("Rename directory and run magit status" . akirak/rename-git-repository-and-status)
    ("Dired" . dired)
    ("Term" . (lambda (dir)
                (let ((default-directory dir))
                  (vterm))))))

(provide 'my/helm/action/git)
