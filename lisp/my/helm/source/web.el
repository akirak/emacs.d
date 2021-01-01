(require 'my/helm/action/web)

(defclass akirak/helm-source-web-dummy (helm-source-dummy)
  ((action :initform 'akirak/helm-web-dummy-source-actions)))

(defclass akirak/helm-source-sync-web-query (helm-source-sync)
  ((action :initform 'akirak/helm-web-dummy-source-actions)))

(defclass akirak/helm-source-browser-bookmarks (helm-source-sync)
  ((candidates :initform 'akirak/browse-url-bookmarks)
   (action :initform '(("Browse" . akirak/browse-url-or-query)))))

(defun akirak/helm-window-visible-url-candidates (window)
  (let ((buffer (window-buffer window))

        (start (window-start window))
        (end (window-end window))
        result)
    (with-current-buffer buffer
      (save-excursion
        (goto-char start)
        (while (re-search-forward (rx "http" (?  "s") "://") end t)
          (when-let (url (or (thing-at-point 'url)
                             (url-get-url-at-point)))
            (push (cons (format "%s (%s)" url (buffer-name buffer))
                        url)
                  result)))))
    (nreverse result)))

(defun akirak/helm-web-sources ()
  (list (helm-build-sync-source "URLs in visible windows"
          :candidates
          (->> (window-list)
               (-map #'akirak/helm-window-visible-url-candidates)
               (-flatten-n 1))
          :action 'akirak/helm-web-dummy-source-actions)
        (helm-make-source "Browser bookmarks"
            'akirak/helm-source-browser-bookmarks)
        (helm-make-source "Query history"
            'akirak/helm-source-sync-web-query
          :candidates 'akirak/web-query-history)
        (helm-make-source "URL or query (open in browser)"
            'akirak/helm-source-web-dummy)))

(provide 'my/helm/source/web)
