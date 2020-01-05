(use-package clipurl
  :straight (clipurl :host github :repo "akirak/clipurl.el"))

(use-package ivy-clipurl
  :straight clipurl
  :commands (ivy-clipurl)
  :config
  (ivy-add-actions 'ivy-clipurl
                   '(("C" akirak/git-clone-url "git clone")
                     ("r" org-web-tools-read-url-as-org "Read as Org")
                     ("u" akirak/straight-use-package-git-url "use-package")
                     ("cc" akirak/capture-url-as-link-item "Append item to clock")
                     ("cb" akirak/org-capture-url-to-bookmark "Bookmark to bookmark"))))

(defun akirak/capture-url-as-link-item (url)
  (akirak/org-capture-item
   (org-web-tools--org-link-for-url url)))

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
