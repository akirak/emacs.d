(require 'my/helm/action/generic)

(cl-defmethod akirak/helm-action-switch-same-window ((object (head file)))
  (find-file (cdr object)))

(cl-defmethod akirak/helm-action-switch-other-window ((object (head file)))
  (find-file-other-window (cdr object)))

(cl-defmethod akirak/helm-action-dired-jump ((object (head file)))
  (dired-jump nil (cdr object)))

(cl-defmethod akirak/helm-action-find-file-interactively ((object (head file)))
  (counsel-find-file (cdr object)))

(defconst akirak/helm-file-actions
  akirak/helm-switch-actions-default-same-window)

(provide 'my/helm/action/file)
