(defvar akirak/helm-org-ql-buffers-files nil)

(defclass akirak/helm-source-org-ql-src-block (helm-source-sync)
  ((candidates
    :initform (lambda ()
                (let* ((query `(and (src :lang ,akirak/programming-recipe-mode-name)
                                    ,(org-ql--plain-query helm-pattern)))
                       (window-width (window-width (helm-window))))
                  (ignore-errors
                    ;; Ignore errors that might be caused by partially typed queries.
                    (org-ql-select akirak/helm-org-ql-buffers-files query
                      :action
                      `(-let (((path . marker)
                               (helm-org-ql--heading ,window-width))
                              (src (when (re-search-forward org-babel-src-block-regexp nil t)
                                     (let ((end (point)))
                                       (org-babel-goto-src-block-head)
                                       (org-element-property :value (org-element-src-block-parser end nil))))))
                         (cons (concat path
                                       (pcase (and src (-filter #'s-present-p (s-lines src)))
                                         ('() "")
                                         (`(,head . ,tail)
                                          (concat (propertize
                                                   "\n>>> "
                                                   'face 'font-lock-comment-face)
                                                  (propertize
                                                   (string-trim-left head)
                                                   'face 'font-lock-string-face)
                                                  (if tail
                                                      (propertize
                                                       (format " ... (%d more lines)" (length tail))
                                                       'face 'font-lock-comment-face)
                                                    "")))))
                               marker)))))))
   (match :initform #'identity)
   (fuzzy-match :initform nil)
   (multimatch :initform nil)
   (nohighlight :initform t)
   (volatile :initform t)
   (persistent-action :initform #'akirak/helm-org-narrow-to-subtree-action
                      :initarg :persistent-action)))

(cl-defun akirak/helm-org-src-block-source (&key lang action persistent-action)
  (helm-make-source "Recipes" 'helm-source-sync
    :candidates
    :persistent-action (or persistent-action #'akirak/helm-org-narrow-to-subtree-action)
    :action (or action #'akirak/helm-yankpad-expand-src-action)))

(provide 'my/helm/source/org)
