(use-package frog-jump-buffer
  :commands (frog-jump-buffer)
  :custom
  (frog-jump-buffer-default-filter #'akirak/buffer-same-project-p))

(defun akirak/switch-to-project-file-buffer (project)
  (interactive (list (-some-> (project-current)
                       (project-roots)
                       (car-safe))))
  (cl-check-type project file-directory)
  (cl-labels ((root-of (buffer)
                       (-some-> (project-current nil (buffer-dir buffer))
                         (project-roots)
                         (car-safe)))
              (buffer-dir (buffer)
                          (buffer-local-value 'default-directory buffer))
              (format-mode (buffer)
                           (format "[%s]" (buffer-local-value 'major-mode buffer)))
              (format-fbuf (buffer)
                           (let ((root (root-of buffer))
                                 (file (buffer-file-name buffer))
                                 (modified (buffer-modified-p buffer)))
                             (concat (if modified "* " "")
                                     (if root
                                         (format "%s > %s "
                                                 (f-short root)
                                                 (and root (f-relative file root)))
                                       (f-short file))
                                     " "
                                     (format-mode buffer))))
              (same-project-p (buf)
                              (file-equal-p project (root-of buf)))
              (project-bufp (buf)
                            (not (f-ancestor-of-p "~/lib/" (buffer-file-name buf))))
              (file-buffer-cell (buffer)
                                (cons (format-fbuf buffer) buffer)))
    (-let* ((file-buffers (-filter #'buffer-file-name (buffer-list)))
            ((same-project-buffers other-file-buffers)
             (-separate #'same-project-p file-buffers))
            (other-project-buffers (-filter #'project-bufp other-file-buffers))
            (other-projects (-> (-map #'root-of other-project-buffers)
                                (-uniq))))
      (helm :prompt "Project file buffers: "
            :sources
            (list (helm-build-sync-source (format "File buffers in project %s"
                                                  project)
                    :candidates (mapcar #'file-buffer-cell same-project-buffers))
                  (helm-build-sync-source "File buffers in other projects"
                    :candidates (mapcar #'file-buffer-cell other-project-buffers))
                  (helm-build-sync-source "Other projects with open file buffers"
                    :candidates other-projects)
                  (helm-build-sync-source "Recentf"
                    :candidates (-map #'f-short recentf-list))
                  (helm-build-sync-source "Git repositories"
                    :candidates (->> (magit-repos-alist)
                                     (-map #'cdr)
                                     (-map #'f-short))
                    :action '(("Switch to project" . akirak/switch-to-project-file-buffer)
                              ("Magit status" . magit-status))))))))

(general-def "C-x p" #'akirak/switch-to-project-file-buffer)

(provide 'setup-switch-buffer)
