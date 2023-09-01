(require 'my/helm/source/dir)
(require 'my/helm/action/dir)
(require 'my/gitmodule/enum)

(defclass akirak/helm-source-gitmodule (helm-source-sync)
  ((action :initform 'akirak/helm-gitmodule-actions-1)))

(defconst akirak/helm-toplevel-repos-submodules-source
  (helm-make-source "Git submodules of top-level repositories in ~"
      akirak/helm-source-gitmodule
    :candidates (lambda ()
                  (-map (lambda (pairs)
                          (cons (akirak/helm-format-gitmodule pairs)
                                pairs))
                        (akirak/toplevel-repos-submodules)))))

(defun akirak/helm-format-gitmodule (pairs)
  (let ((description (alist-get 'description pairs))
        (active (alist-get 'active pairs))
        (root (alist-get 'root pairs))
        (path (alist-get 'path pairs))
        (tags (akirak/git-module-tags pairs)))
    (concat (f-slash (f-short root))
            (propertize (f-short path)
                        'font-lock-face (if active
                                            'font-lock-builtin-face
                                          'font-lock-comment-face))
            "  "
            (if tags
                (propertize (format "[%s]  " (string-join tags " "))
                            'font-lock-face 'font-lock-keyword-face)
              "")
            (or description ""))))

(provide 'my/helm/source/gitmodule)
