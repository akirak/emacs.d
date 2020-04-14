(require 'my/dir/function)

(defconst akirak/helm-git-project-actions
  '(("Magit status" . magit-status)
    ("Rename directory" . akirak/rename-git-repository)))

(provide 'my/helm/action/git)
