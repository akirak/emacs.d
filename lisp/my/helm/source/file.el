(require 'my/file/enum)
(require 'my/helm/action/file)

(defclass akirak/helm-source-file (helm-source-sync)
  ((action :initform 'akirak/helm-switch-actions-default-same-window)
   (coerce :initform (lambda (path) `(file . ,path)))))

;; The project root is default-directory
(defclass akirak/helm-source-project-file (akirak/helm-source-file)
  ((candidates
    :initform
    (lambda ()
      (let* ((root (f-short default-directory))
             (open-files (->> (buffer-list)
                              (-map
                               (lambda (buffer)
                                 (let* ((file (buffer-file-name buffer))
                                        (file (and file (f-short file))))
                                   (and file
                                        (string-prefix-p root file)
                                        (string-remove-prefix root file)))))
                              (delq nil))))
        (-map (lambda (file)
                (if (cl-member file open-files :test #'string-equal)
                    (propertize file 'face 'link-visited)
                  file))
              (akirak/project-files default-directory :sort 'modified)))))))

(defconst akirak/helm-source-project-files
  (helm-make-source "Files in project" 'akirak/helm-source-project-file
    :persistent-action (-compose #'akirak/magit-log-file #'cdr)))

(defconst akirak/helm-source-org-starter-known-files
  (helm-make-source "org-starter-known-files" 'akirak/helm-source-file
    :candidates (lambda () (mapcar #'f-short org-starter-known-files))))

(defconst akirak/helm-source-recent-files
  (helm-make-source "Recent files" 'akirak/helm-source-file
    :candidates (lambda () (-map #'f-short recentf-list))))

(defconst akirak/helm-source-dummy-find-file
  (helm-build-dummy-source "File from the default directory"
    :action #'find-file))

;; (defvar akirak/git-status-source
;;   (helm-build-sync-source "Git status"
;;     :candidates (lambda () (process-lines "git" "status" "--short"))
;;     :persistent-action
;;     (lambda (status)
;;       (let ((file (status-file status)))
;;         (with-current-buffer (or (find-buffer-visiting file)
;;                                  (find-file-noselect file))
;;           (magit-diff-buffer-file))))
;;     :action '(("Find file" . (lambda (status)
;;                                (let ((relative (status-file status)))
;;                                  (find-file (f-join root relative))))))))

(provide 'my/helm/source/file)
