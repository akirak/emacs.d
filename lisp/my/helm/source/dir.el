(defclass akirak/helm-source-directory (helm-source-sync)
  ((action :initform 'akirak/helm-directory-actions-1)))

(defconst akirak/helm-magic-list-repos-source
  (helm-make-source "magit-list-repos" 'akirak/helm-source-directory
    :candidates (lambda () (mapcar #'f-short (magit-list-repos)))))

(defclass akirak/helm-source-magit-repos (akirak/helm-source-directory)
  ((candidates :initform (lambda () (->> (magit-repos-alist)
                                         (-map #'cdr)
                                         (-map #'f-short))))))

(defun akirak/helm-project-root-and-ancestors-source (root)
  (helm-make-source "Project root and its ancestors" 'akirak/helm-source-directory
    :candidates (lambda () (akirak/directory-self-and-ancestors root))))

(defconst akirak/helm-open-buffer-directories-source
  (helm-make-source "Directories of open buffers" 'akirak/helm-source-directory
    :candidates (lambda () (akirak/open-buffer-directories))))

(provide 'my/helm/source/dir)
