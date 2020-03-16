(require 'my/helm/action/generic)

(defmethod akirak/helm-action-switch-same-window ((obj buffer))
  (switch-to-buffer obj))

(defmethod akirak/helm-action-switch-other-window ((obj buffer))
  (switch-to-buffer-other-window obj))

(defconst akirak/helm-buffer-actions-1
  (append akirak/helm-switch-actions-default-switch-select
          '(("Kill buffer" . kill-buffer))))

(provide 'my/helm/action/buffer)
