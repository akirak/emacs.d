(defun akirak/helm-project-file-source (root)
  (cl-labels ((status-file (status) (substring status 3)))
    (let* ((attrs (file-attributes root))
           (mtime (nth 5 attrs))
           (cache (assoc root akirak/directory-contents-cache))
           (default-directory root)
           (contents (if (or (not (cdr cache))
                             (time-less-p (cadr cache) mtime))
                         (let* ((items (process-lines "rg" "--files"
                                                      "--color=never"
                                                      "--sortr" "modified"))
                                (cell (cons mtime items)))
                           (if cache
                               (setf (cdr cache) cell)
                             (push (cons root cell) akirak/directory-contents-cache))
                           items)
                       (cddr cache)))
           (open-files (->> (buffer-list)
                            (-map (lambda (buffer)
                                    (let* ((file (buffer-file-name buffer))
                                           (file (and file (f-short file)))
                                           (root (f-short root)))
                                      (and file
                                           (string-prefix-p root file)
                                           (string-remove-prefix root file)))))
                            (delq nil))))
      (helm-build-sync-source "Files"
        :candidates (->> contents
                         (-map (lambda (file)
                                 (if (cl-member file open-files :test #'string-equal)
                                     (propertize file 'face 'link-visited)
                                   file))))
        :persistent-action
        (lambda (relative)
          (let ((file (f-join root relative)))
            (akirak/magit-log-file file)))
        :action (lambda (relative)
                  (find-file (f-join root relative)))))))

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

(provide 'akirak/helm/source/file)
