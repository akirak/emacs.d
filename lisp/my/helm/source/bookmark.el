(require 'helm-bookmark)

;; Based on `helm-source-bookmark-files&dirs' in helm-bookmark.el
(defconst akirak/helm-directory-bookmark-source
  (helm-make-source "Directory bookmarks" 'helm-source-filtered-bookmarks
    :init (lambda ()
            (bookmark-maybe-load-default-file)
            (helm-init-candidates-in-buffer
                'global (helm-bookmark-filter-setup-alist
                         (lambda (bookmark)
                           (let* ((filename (bookmark-get-filename bookmark))
                                  (isnonfile (equal filename helm-bookmark--non-file-filename)))
                             (and filename
                                  (not isnonfile)
                                  (string-suffix-p "/" filename)
                                  (not (bookmark-get-handler bookmark))))))))))

(provide 'my/helm/source/bookmark)
