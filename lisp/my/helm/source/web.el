(require 'my/helm/action/web)

(defclass akirak/helm-source-web-dummy (helm-source-dummy)
  ((action :initform 'akirak/helm-web-dummy-source-actions)))

(defclass akirak/helm-source-sync-web-query (helm-source-sync)
  ((action :initform 'akirak/helm-web-dummy-source-actions)))

(defclass akirak/helm-source-browser-bookmarks (helm-source-sync)
  ((candidates :initform 'akirak/browse-url-bookmarks)
   (action :initform '(("Browse" . akirak/browse-url-or-query)))))

(defun akirak/helm-web-sources ()
  (list (helm-make-source "Browser bookmarks"
            'akirak/helm-source-browser-bookmarks)
        (helm-make-source "Query history"
            'akirak/helm-source-sync-web-query
          :candidates 'akirak/web-query-history)
        (helm-make-source "URL or query (open in browser)"
            'akirak/helm-source-web-dummy)))

(provide 'my/helm/source/web)
