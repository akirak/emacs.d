(require 'my/helm/action/dir)
(require 'my/helm/action/git)

(defclass akirak/helm-source-directory (helm-source-sync)
  ((action :initform 'akirak/helm-directory-actions-1)))

(defconst akirak/helm-magic-list-repos-source
  (helm-make-source "magit-list-repos" 'akirak/helm-source-directory
    :candidates (lambda () (mapcar #'f-short (magit-list-repos)))))

(defclass akirak/helm-source-magit-repos (akirak/helm-source-directory)
  ((candidates :initform (lambda () (->> (magit-repos-alist)
                                         (-map #'cdr)
                                         (-map #'f-short))))
   (action :initform 'akirak/helm-git-project-actions)))

(defconst akirak/helm-project-root-and-ancestors-source
  (helm-make-source "Project root and its ancestors" 'akirak/helm-source-directory
    :candidates (lambda ()
                  (require 'my/dir/enum)
                  (akirak/directory-self-and-ancestors default-directory))))

(defconst akirak/helm-open-buffer-directories-source
  (helm-make-source "Directories of open buffers" 'akirak/helm-source-directory
    :candidates (lambda ()
                  (require 'my/dir/enum)
                  (akirak/open-buffer-directories))))

(provide 'my/helm/source/dir)
