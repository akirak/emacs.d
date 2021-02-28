(require 'my/helm/source/buffer)
(require 'my/helm/source/file)
(require 'my/helm/source/dir)

(defconst akirak/project-directory-regexp
  ;; Precalculate the pattern to prevent extensive string manipulations.
  (rx bol
      (eval (expand-file-name "~/"))
      (or "projects"
          "work"
          ".config")
      "/"))

(defun akirak/helm-project-buffer-sources (project switch-to-project-fn)
  (cl-labels ((root-of (buffer)
                       (akirak/project-root (buffer-dir buffer)))
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
                              (when-let* ((root (root-of buf)))
                                (equal default-directory root)))
              (project-bufp (buf)
                            (string-match-p akirak/project-directory-regexp
                                            (buffer-file-name buf)))
              (file-buffer-cell (buffer)
                                (cons (format-fbuf buffer) buffer))
              (kill-project-bufs (default-directory)
                                 (let ((bufs (-filter (lambda (buf)
                                                        (let ((dir (buffer-dir buf)))
                                                          (or (f-equal-p dir default-directory)
                                                              (f-ancestor-of-p default-directory dir))))
                                                      (buffer-list))))
                                   (when (yes-or-no-p (format "Kill all buffers in %s" default-directory))
                                     (mapc #'kill-buffer bufs)
                                     (helm-run-after-quit
                                      (lambda ()
                                        (funcall switch-to-project-fn default-directory)))))))
    (-let* ((file-buffers (->> (buffer-list)
                               (-filter (lambda (buf)
                                          (when-let (filename (buffer-file-name buf))
                                            (not (recentf-include-p filename)))))))
            ((same-project-buffers other-file-buffers)
             (if project (-separate #'same-project-p file-buffers) (list nil file-buffers)))
            (same-project-other-buffers
             (-remove-item (current-buffer) same-project-buffers))
            (other-project-buffers (-filter #'project-bufp other-file-buffers))
            (other-projects (->> (-map #'root-of other-project-buffers)
                                 (delq nil)
                                 (-uniq))))
      (list (cond
             (same-project-buffers
              (helm-make-source (format "File buffers in project %s"
                                        default-directory)
                  'akirak/helm-source-buffer
                :candidates (mapcar #'file-buffer-cell
                                    (or same-project-other-buffers
                                        same-project-buffers))))
             (project akirak/helm-source-project-files))
            (helm-make-source "File buffers in other projects"
                'akirak/helm-source-buffer
              :candidates (mapcar #'file-buffer-cell other-project-buffers))
            (helm-make-source "Other projects with open file buffers"
                'akirak/helm-source-directory
              :candidates other-projects
              :persistent-action #'kill-project-bufs
              :action `(("Switch to project" . ,switch-to-project-fn)
                        ("Magit status" . magit-status)))))))

(provide 'my/helm/source/complex)
