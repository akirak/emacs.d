(require 'helm-bookmark)
(require 'my/helm/source/dir)

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

(defconst akirak/helm-directory-bookmark-as-git-source
  (helm-make-source "Bookmarks for Git repos" 'helm-source-filtered-bookmarks
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
                                  (not (bookmark-get-handler bookmark))
                                  (file-directory-p (concat filename ".git"))
                                  ))))))
    :action 'akirak/helm-git-project-actions))

(defconst akirak/helm-source-remote-bookmark
  (helm-make-source "Remote bookmarks" 'helm-source-filtered-bookmarks
    :init (lambda ()
            (bookmark-maybe-load-default-file)
            (helm-init-candidates-in-buffer
                'global (helm-bookmark-filter-setup-alist
                         (lambda (bookmark)
                           (let* ((filename (bookmark-get-filename bookmark))
                                  (isnonfile (equal filename helm-bookmark--non-file-filename)))
                             (and filename
                                  (not isnonfile)
                                  (file-remote-p filename)))))))))

(provide 'my/helm/source/bookmark)
