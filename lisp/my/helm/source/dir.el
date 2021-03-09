(require 'my/helm/action/dir)

(defclass akirak/helm-source-directory (helm-source-sync)
  ((action :initform 'akirak/helm-directory-actions-1)))

(defclass akirak/helm-source-git-repository (helm-source-sync)
  ((action :initform 'akirak/helm-git-project-actions)))

(defconst akirak/helm-magit-list-repos-source
  (helm-make-source "magit-list-repos" 'akirak/helm-source-git-repository
    :candidates
    (lambda ()
      (-let* ((repos (-map #'f-short (magit-list-repos)))
              (buffer-repos (->> (buffer-list)
                                 (-map (lambda (buf) (buffer-local-value 'default-directory buf)))
                                 (delq nil)
                                 (-map #'akirak/project-root)
                                 (cl-delete-duplicates)))
              ((open-repos closed-repos)
               (-separate (lambda (repo)
                            (member repo buffer-repos))
                          repos)))
        (append (-map (lambda (repo)
                        (propertize repo 'face 'font-lock-builtin-face))
                      open-repos)
                closed-repos)))))

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

(defconst akirak/helm-open-file-buffer-directories-source
  (helm-make-source "Directories of open file buffers" 'akirak/helm-source-directory
    :candidates (lambda ()
                  (require 'my/dir/enum)
                  (akirak/open-file-buffer-directories))))

(defconst akirak/helm-project-parent-directory-source
  (helm-make-source "Project parent directories" 'akirak/helm-source-directory
    :candidates #'akirak/project-parent-directories))

(provide 'my/helm/source/dir)
