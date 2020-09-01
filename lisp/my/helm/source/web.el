(require 'my/helm/action/web)

(defclass akirak/helm-source-web-dummy (helm-source-dummy)
  ((action :initform 'akirak/helm-web-dummy-source-actions)))

(provide 'my/helm/source/web)
