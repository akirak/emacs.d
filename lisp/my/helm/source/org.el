(require 'my/helm/action/org-marker)

(defvar akirak/helm-org-ql-buffers-files nil)

(defconst akirak/helm-org-posframe-temporary-buffer "org posframe")

(defclass akirak/helm-source-org-ql-src-block (helm-source-sync)
  ((candidates
    :initform (lambda ()
                (let* ((query `(and (src :lang ,akirak/programming-recipe-mode-name)
                                    ,(org-ql--query-string-to-sexp helm-pattern)))
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
   (cleanup :initform (lambda ()
                        (when (get-buffer akirak/helm-org-posframe-temporary-buffer)
                          (posframe-delete akirak/helm-org-posframe-temporary-buffer))))
   (persistent-action :initform #'akirak/helm-org-narrow-to-subtree-action
                      :initarg :persistent-action)))

(defvar akirak/helm-org-planning-items-source
  (helm-make-source "Org planning" 'helm-source-sync
    :candidates
    (lambda ()
      (->> (org-ql-select (org-agenda-files)
             '(and (planning :to 0)
                   (not (done)))
             :sort 'date
             :action 'element-with-markers)
           (-map (lambda (x)
                   (let ((marker (or (org-element-property :org-marker x)
                                     (org-element-property :org-hd-marker x))))
                     (cons (org-ql-view--format-element x)
                           marker))))))
    :action
    'helm-org-headings-actions))

(provide 'my/helm/source/org)
