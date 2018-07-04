;;; init-web-search.el --- Search tools -*- lexical-binding: t -*-

(defun akirak/web-search-firefox (term)
  (interactive (list (read-from-minibuffer "Search term: ")))
  (start-process "firefox-search" nil "firefox" "--search" term ))

;;;; surfraw

(defmacro akirak/define-surfraw-command (engine)
  (let ((func-name (intern (concat "akirak/surfraw/" engine))))
    `(defun ,func-name (pattern)
       ,(format "Search PATTERN using %s from surfraw." engine)
       (interactive (list (read-string ,(format "%s: " engine)
                                       nil nil
                                       (if (use-region-p)
                                           (buffer-substring-no-properties
                                            (region-beginning) (region-end))
                                         (thing-at-point 'symbol)))))
       (require 'helm-net)
       (helm-surfraw pattern ,engine))))

(akirak/define-surfraw-command "google")
(akirak/define-surfraw-command "duckduckgo")

;;;; Extensive search engines

(defun akirak/helm-search (query)
  "Choose a search engine for QUERY."
  (interactive
   (let* ((default (if (use-region-p)
                       (buffer-substring-no-properties
                        (region-beginning) (region-end))
                     (thing-at-point 'symbol)))
          (prompt (if default
                      (format "SearchFor (default %s): " default)
                    "SearchFor: ")))
     (read-string prompt nil 'helm-surfraw-input-history default)))
  (require 'helm-net)
  (helm :prompt (format "Search engine for \"%s\": " query)
        :sources
        (list (helm-build-sync-source "Surfraw recent"
                :candidates 'helm-surfraw-engines-history
                :action
                (lambda (engine) (helm-surfraw query engine)))
              (helm-build-sync-source "Surfraw"
                :candidates (helm-build-elvi-list)
                :action
                (lambda (elvi)
                  (helm-surfraw query (car (split-string elvi))))))))

(defun akirak/helm-search-symbol-at-point ()
  (interactive)
  (akirak/helm-search (thing-at-point 'symbol)))

(provide 'init-web-search)
;;; init-web-search.el ends here
