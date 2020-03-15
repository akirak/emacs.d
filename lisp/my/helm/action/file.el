(require 'my/helm/action/generic)

(cl-defmethod akirak/helm-action-switch-same-window ((object (head file)))
  (find-file (cdr object)))

(cl-defmethod akirak/helm-action-switch-other-window ((object (head file)))
  (find-file-other-window (cdr object)))

(defconst akirak/helm-file-actions
  akirak/helm-switch-actions-default-same-window)

(provide 'my/helm/action/file)
