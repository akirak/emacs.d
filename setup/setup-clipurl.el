(use-package clipurl
  :straight (clipurl :host github :repo "akirak/clipurl.el"))

(defmacro akirak/def-org-capture-url-to-toplevel (filename &optional template-fn)
  `(defun ,(intern (format "akirak/org-capture-url-to-%s-toplevel"
                           (file-name-base filename)))
       (url)
     (if-let ((file (org-starter-locate-file ,filename nil t)))
         (let ((org-capture-entry `("b" "Bookmark" entry (file ,file)
                                    ,(funcall (or ,template-fn #'akirak/org-capture-url-bookmark-template) url))))
           (org-capture))
       (user-error "File not found: %s" ,filename))))

(defmacro akirak/def-org-capture-url-to-reverse-datetree (filename &optional template-fn)
  `(defun ,(intern (format "akirak/org-capture-url-to-%s-datetree"
                           (file-name-base filename)))
       (url)
     (if-let ((file (org-starter-locate-file ,filename nil t)))
         (let ((org-capture-entry `("b" "Bookmark" entry (file+function ,file org-reverse-datetree-goto-date-in-file)
                                    ,(funcall (or ,template-fn #'akirak/org-capture-url-bookmark-template) url))))
           (org-capture))
       (user-error "File not found: %s" ,filename))))

(use-package ivy-clipurl
  :straight clipurl
  :commands (ivy-clipurl)
  :config
  (ivy-add-actions 'ivy-clipurl
                   '(("cj" akirak/org-journal-capture-url-as-heading "Capture to journal as a heading")
                     ("rb" akirak/org-capture-book-url "Enqueue book")
                     ("ra" akirak/org-capture-news-article-url "Enqueue article")
                     ("rv" akirak/org-capture-screencast-url "Enqueue video (screencast)")
                     ("rr" akirak/org-capture-url-refile "Refile target")
                     ("rt" akirak/org-capture-url-as-todo-refile "Refile target (as a todo)"))))

(cl-defun akirak/org-capture-url-bookmark-template (url &key
                                                        todo
                                                        default-title
                                                        manual-tags
                                                        as-body)
  (declare (indent 1))
  (let* ((html (with-timeout (3)
                 (org-web-tools--get-url url)))
         (title (if default-title
                    (org-web-tools--html-title html)
                  (read-string (format "Title of %s: " url)
                               (when html
                                 (org-web-tools--html-title html))
                               nil nil t)))
         (link (org-link-make-string url title))
         (drawer "\n:PROPERTIES:\n:CREATED_TIME: %U\n:END:\n"))
    (if as-body
        (concat "* "
                (if todo "TODO " "")
                "%^{Title of the entry}" drawer "%?\n\n" link)
      (concat "* " (if todo "TODO " "") link " :link:" (if manual-tags "%^g" "")
              drawer "\n%?"))))

(cl-defun akirak/org-capture-url-as-heading (destination url &key immediate default-title)
  (declare (indent 1))
  (let* ((template (akirak/org-capture-url-bookmark-template url
                     :default-title default-title))
         (org-capture-entry `("u" "Url" entry ,destination
                              ,template :immediate ,(not (null immediate)))))
    (org-capture)))

(defun akirak/org-capture-url-as-body (destination url)
  (declare (indent 1))
  (let* ((template (akirak/org-capture-url-bookmark-template url :as-body t))
         (org-capture-entry `("u" "Url" entry
                              ,destination
                              ,template)))
    (org-capture)))

(defun akirak/org-capture-link-to-custom-id (custom-id url)
  (let* ((file (org-starter-locate-file "practice.org" nil t))
         (location-function `(lambda () (akirak/org-goto-custom-id ,custom-id)))
         (template (akirak/org-capture-url-bookmark-template url
                     :manual-tags t :default-title t))
         (org-capture-entry `("u" "Url" entry
                              (file+function ,file ,location-function)
                              ,template :clock-in t :clock-resume t)))
    (org-capture)))

(defalias 'akirak/org-capture-screencast-url
  (-partial #'akirak/org-capture-link-to-custom-id "screencast-inbox"))
(defalias 'akirak/org-capture-book-url
  (-partial #'akirak/org-capture-link-to-custom-id "book-inbox"))
(defalias 'akirak/org-capture-news-article-url
  (-partial #'akirak/org-capture-link-to-custom-id "news-inbox"))

(cl-defun akirak/org-capture-url-refile (url &key as-todo)
  (let* ((location (helm :prompt "Capture location: "
                         :sources
                         (helm-build-sync-source "Refile targets"
                           :candidates
                           (->> (org-refile-get-targets)
                                (-map (pcase-lambda (`(,caption ,file . ,_))
                                        (let ((path (if (string-prefix-p file caption)
                                                        (string-remove-prefix file caption)
                                                      "")))
                                          (cons (concat (file-name-nondirectory file) path)
                                                (cons file
                                                      (cdr (split-string path "/")))))))))))
         (template (akirak/org-capture-url-bookmark-template url
                     :todo as-todo :default-title t))
         (org-capture-entry `("u" "Url" entry
                              (file+olp ,(car location) ,@(cdr location))
                              ,template)))
    (org-capture)))

(defun akirak/org-capture-url-as-todo-refile (url)
  (akirak/org-capture-url-refile url :as-todo t))

(defun akirak/org-journal-capture-url-as-heading (url)
  (akirak/org-capture-url-as-heading '(function org-journal-find-location)
    url :default-title t))

(defalias 'akirak/org-journal-capture-url-as-body
  (-partial #'akirak/org-capture-url-as-body '(function org-journal-find-location)))

;; Deprecated.
(defun akirak/org-capture-url-to-bookmark (url &optional marker)
  (declare (indent 1))
  (interactive "sUrl: ")
  (cond
   (marker
    (let ((org-capture-entry
           `("b" "Bookmark" entry
             (function (lambda () (org-goto-marker-or-bmk ,marker)))
             ,(akirak/org-capture-url-bookmark-template url))))
      (org-capture)))
   (t
    (ivy-read "Bookmark destination: "
              (ivy-omni-org--bookmarks)
              :caller 'akirak/org-capture-bookmark-destination
              :action
              `(lambda (bookmark)
                 (akirak/org-capture-url-to-bookmark ,url
                   (save-window-excursion
                     (bookmark-jump bookmark)
                     (point-marker))))))))

(defun akirak/org-capture-url-to-avy (url)
  (interactive)
  (let* ((marker (avy-with avy-goto-line
                   (avy-jump (rx bol (1+ "*") (1+ space)))
                   :action (point-marker)))
         (org-capture-entry
          `("b" "Bookmark" entry
            (function (lambda () (org-goto-marker-or-bmk ,marker)))
            ,(akirak/org-capture-url-bookmark-template url)
            ;; Unlike the case of a bookmark destination, this
            ;; finishes immediately, because the entry is already
            ;; visible.
            :immediate-finish t)))
    (org-capture)))

;; Deprecated.
(defun akirak/org-capture-url-to-here (url)
  (interactive)
  (cl-assert (derived-mode-p 'org-mode))
  (cl-assert (not (org-before-first-heading-p)))
  (let ((org-capture-entry
         `("b" "Bookmark" entry
           (function (lambda () (org-back-to-heading)))
           ,(akirak/org-capture-url-bookmark-template url
                                                      :default-title t))))
    (org-capture)))

(provide 'setup-clipurl)
