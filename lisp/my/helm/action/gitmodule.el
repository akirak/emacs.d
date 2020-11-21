(require 'my/helm/action/dir)
(require 'my/helm/action/git)

(defconst akirak/helm-gitmodule-actions-1
  '(("Magit status" . magit-status)
    ("Magit log all" . (lambda (dir)
                         (let ((default-directory dir))
                           (magit-log-all))))
    ("Find files recursively" . akirak/find-file-recursively)
    ("Dired" . dired)
    ("Term" . (lambda (dir)
                (let ((default-directory dir))
                  (vterm))))
    ("Rename directory and run magit status"
     . akirak/rename-git-repository-and-status)))

(provide 'my/helm/action/gitmodule)
