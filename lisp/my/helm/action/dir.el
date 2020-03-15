(require 'my/helm/action/generic)

(defconst akirak/helm-directory-actions-1
  '(("Dired" . dired)
    ("Find file" . counsel-find-file)
    ("Term" . (lambda (dir)
                (let ((default-directory dir))
                  (vterm))))))

(provide 'my/helm/action/dir)
