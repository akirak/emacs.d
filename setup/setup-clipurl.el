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
                     ("cp" akirak/org-journal-capture-url-as-body "Capture to journal as body")
                     ("ca" akirak/org-capture-url-to-avy "Bookmark to avy"))))

(cl-defun akirak/org-capture-url-bookmark-template (url &key
                                                        default-title
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
        (concat "* %^{Title of the entry}" drawer "%?\n\n" link)
      (concat "* " link " :link:" drawer "%?"))))

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
