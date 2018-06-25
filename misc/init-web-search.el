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

(provide 'init-web-search)
;;; init-web-search.el ends here
