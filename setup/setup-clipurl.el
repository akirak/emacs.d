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
                   `(("cl" ,(akirak/def-org-capture-url-to-toplevel "library.org")
                      "Bookmark to library")
                     ("cc" ,(akirak/def-org-capture-url-to-reverse-datetree "cpb.org")
                      "Bookmark to cpb")
                     ("cn" ,(akirak/def-org-capture-url-to-toplevel "nixrepos.org")
                      "Bookmark to nixrepos")
                     ("cb" akirak/org-capture-url-to-bookmark "Bookmark to Org bookmark"))))

(defun akirak/org-capture-url-bookmark-template (url)
  (let* ((html (with-timeout (3)
                 (org-web-tools--get-url url)))
         (title (read-string "Title: "
                             (when html
                               (org-web-tools--html-title html))
                             nil nil t)))
    (concat "* " (org-make-link-string url title) " %^g"
            "\n:PROPERTIES:
:CREATED_TIME: %U
:END:
%?")))

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

(provide 'setup-clipurl)
