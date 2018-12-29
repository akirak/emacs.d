;;; init-web-search.el --- Search tools -*- lexical-binding: t -*-

(defmacro akirak/define-search-engine (id name url)
  (declare (indent 1))
  (let ((func-name (intern (concat "akirak/search/" (symbol-name id)))))
    `(defun ,func-name (query)
       ,(format "Search QUERY using %s." name)
       (interactive ,(format "M%s: " name))
       (browse-url (format ,url query)))))

(defcustom akirak/search-engine-alist
  '((lucky
     "Google (I'm Feeling Lucky)"
     "http://www.google.com/webhp?#q=%s&btnI=I"))
  "Alist of search engines."
  :type
  '(repeat (list (symbol :tag "Identifier")
                 (string :tag "Caption")
                 (string :tag "URL with a placeholder")))
  :set (lambda (symbol value)
         (set-default symbol value)
         (cl-loop for (id name url . _) in value
                  do (let ((func-name (intern (concat "akirak/search/"
                                                      (symbol-name id)))))
                       `(defun ,func-name (query)
                          ,(format "Search QUERY using %s." name)
                          (interactive ,(format "M%s: " name))
                          (browse-url (format ,url query)))))))

(defun akirak/search-engine (id query)
  (interactive
   (let* ((default (if (use-region-p)
                       (buffer-substring-no-properties
                        (region-beginning) (region-end))
                     (thing-at-point 'symbol)))
          (query (read-string (if default
                                  (format "Search (default %s): " default)
                                "Search: ")
                              nil 'helm-surfraw-input-history default))
          (id (helm :prompt (format "Search engine for \"%s\":" query)
                    :sources
                    (list (helm-build-sync-source "My search engines"
                            :candidates
                            (cl-loop for (id name . _) in akirak/search-engine-alist
                                     collect (cons name id)))))))
     (list id query)))
  (browse-url (format (nth 1 (alist-get id akirak/search-engine-alist))
                      query)))

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

;;;; Custom search engines

(defun akirak/escape-url-query (query)
  "Escape QUERY into a hex string."
  (string-join (mapcar #'url-hexify-string
                       (-flatten (mapcar #'split-string query)))
               "+"))

;;;;; Life in Japan

(defun akirak/weblio-japanese-chinese-dictionary (query)
  (interactive "MJapanese/Chinese: ")
  (browse-url (format "https://cjjc.weblio.jp/content/%s" query)))

(defun akirak/wikipedia-japanese (query)
  (interactive "MWikipedia (ja): ")
  (eww (format "https://ja.wikipedia.org/wiki/%s" query)))

(defun akirak/search-twitter-accounts (query)
  (browse-url (format "https://twitter.com/search?f=users&vertical=default&q=%s"
                      query)))

(defun akirak/google-maps-search (query)
  (browse-url (format "http://www.google.com/maps/search/%s" query)))

(defun akirak/baidu-baike-search (query)
  (browse-url (format "https://baike.baidu.com/search/word?word=%s" query)))

;;;; search engines

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
     (list (read-string prompt nil 'helm-surfraw-input-history default))))
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