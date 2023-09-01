(require 'my/file/enum)
(require 'my/helm/action/file)

(defclass akirak/helm-source-file (helm-source-sync)
  ((action :initform 'akirak/helm-switch-actions-default-same-window)
   (coerce :initform (lambda (path) `(file . ,path)))
   (keymap :initform 'akirak/helm-file-like-source-keymap)))

;; The project root is default-directory
(defclass akirak/helm-source-project-file (akirak/helm-source-file)
  ((candidates
    :initform
    (lambda ()
      (let* ((root (f-short default-directory))
             (open-files-with-times (->> (buffer-list)
                                         (-map
                                          (lambda (buffer)
                                            (let* ((file (buffer-file-name buffer))
                                                   (time (buffer-local-value 'buffer-display-time
                                                                             buffer))
                                                   (file (and file (f-short file))))
                                              (and file
                                                   (string-prefix-p root file)
                                                   (cons (string-remove-prefix root file)
                                                         time)))))
                                         (delq nil)))
             (project-files (akirak/project-files default-directory)))
        (append (->> open-files-with-times
                     ;; (-sort (-on #'cdr #'time-less-p))
                     (-map #'car)
                     (-map (lambda (file)
                             (propertize file 'face 'link-visited))))
                (set-difference project-files
                                (-map #'car open-files-with-times))))))))

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
