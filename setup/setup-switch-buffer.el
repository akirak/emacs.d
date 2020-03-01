(use-package frog-jump-buffer
  :commands (frog-jump-buffer)
  :custom
  (frog-jump-buffer-default-filter #'akirak/buffer-same-project-p))

(defsubst akirak/buffer-derived-mode-p (buffer &rest modes)
  (declare (indent 1))
  (apply #'provided-mode-derived-p (buffer-local-value 'major-mode buffer)
         modes))

(defcustom akirak/exwm-browser-class-names
  '("Chromium" "Brave" "Chromium-browser")
  "List of class names of browser windows.")

(defun akirak/reference-buffer-p (buffer)
  ;; Based on the implementation of `derived-mode-p'.
  (or (akirak/buffer-derived-mode-p buffer
        'Info-mode 'help-mode 'helpful-mode 'eww-mode)
      (and (akirak/buffer-derived-mode-p buffer
             'exwm-mode)
           (member (buffer-local-value 'exwm-class-name buf)
                   akirak/exwm-browser-class-names))))


(defvar akirak/switch-buffer-helm-actions
  (quote (("Switch to buffer" .
           (lambda (buffer)
             (when current-prefix-arg
               (ace-window nil))
             (switch-to-buffer buffer)))
          ("Kill buffer" . kill-buffer))))

(defvar akirak/find-file-helm-actions
  (quote (("Find file" .
           (lambda (file)
             (when current-prefix-arg
               (ace-window nil))
             (find-file file))))))

(defvar akirak/directory-contents-cache nil)

(defun akirak/find-file-recursively (root)
  (interactive (list (if current-prefix-arg
                         (read-directory-name "Find files in dir: ")
                       (-some-> (project-current)
                         (project-roots)
                         (car-safe)))))
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
      (helm :prompt (format "Browse %s: " root)
            :sources
            (delq nil
                  (list (helm-build-sync-source "Files"
                          :candidates (->> contents
                                           (-map (lambda (file)
                                                   (if (cl-member file open-files :test #'string-equal)
                                                       (propertize file 'face 'link-visited)
                                                     file))))
                          :action (lambda (relative)
                                    (find-file (f-join root relative))))
                        (when (file-equal-p (magit-toplevel root) root)
                          (helm-build-sync-source "Git status"
                            :candidates (process-lines "git" "status" "--short")
                            :persistent-action
                            (lambda (status)
                              (let ((file (status-file status)))
                                (with-current-buffer (or (find-buffer-visiting file)
                                                         (find-file-noselect file))
                                  (magit-diff-buffer-file))))
                            :action '(("Find file" . (lambda (status)
                                                       (let ((relative (status-file status)))
                                                         (find-file (f-join root relative)))))
                                      ;; TODO: Add an action to commit selected files
                                      ;; ("Commit" . (lambda (_)
                                      ;;               (let ((files (-map #'status-file (helm-marked-candidates))))
                                      ;;                 ()
                                      ;;                 )

                                      ;;               ))
                                      )))))))))

(defvar akirak/helm-project-buffer-map
  (let ((map (copy-keymap helm-map)))
    (define-key map (kbd "M-/")
      (lambda ()
        (interactive)
        (helm-run-after-quit (lambda () (akirak/find-file-recursively project)))))
    map))

(defun akirak/switch-to-project-file-buffer (project)
  (interactive (list (-some-> (project-current)
                       (project-roots)
                       (car-safe))))
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
                              (-some->> (root-of buf)
                                (file-equal-p project)))
              (project-bufp (buf)
                            (not (f-ancestor-of-p "~/lib/" (buffer-file-name buf))))
              (file-buffer-cell (buffer)
                                (cons (format-fbuf buffer) buffer))
              (kill-project-bufs (project)
                                 (let ((bufs (-filter (lambda (buf)
                                                        (let ((dir (buffer-dir buf)))
                                                          (or (f-equal-p dir project)
                                                              (f-ancestor-of-p project dir))))
                                                      (buffer-list))))
                                   (when (yes-or-no-p (format "Kill all buffers in %s" project))
                                     (mapc #'kill-buffer bufs)
                                     (helm-run-after-quit (lambda () (akirak/switch-to-project-file-buffer project)))))))
    (-let* ((file-buffers (-filter #'buffer-file-name (buffer-list)))
            ((same-project-buffers other-file-buffers)
             (if project (-separate #'same-project-p file-buffers) (list nil file-buffers)))
            (other-project-buffers (-filter #'project-bufp other-file-buffers))
            (other-projects (->> (-map #'root-of other-project-buffers)
                                 (delq nil)
                                 (-uniq))))
      (helm :prompt "Project file buffers: "
            :sources
            (list (helm-build-sync-source (format "File buffers in project %s"
                                                  project)
                    :candidates (mapcar #'file-buffer-cell same-project-buffers)
                    :keymap akirak/helm-project-buffer-map
                    :action akirak/switch-buffer-helm-actions)
                  (helm-build-sync-source "File buffers in other projects"
                    :candidates (mapcar #'file-buffer-cell other-project-buffers)
                    :action akirak/switch-buffer-helm-actions)
                  (helm-build-sync-source "Other projects with open file buffers"
                    :candidates other-projects
                    :persistent-action #'kill-project-bufs
                    :action '(("Switch to project" . akirak/switch-to-project-file-buffer)))
                  (helm-build-sync-source "Recentf"
                    :candidates (-map #'f-short recentf-list)
                    :action akirak/find-file-helm-actions)
                  (helm-build-sync-source "Git repositories"
                    :candidates (->> (magit-repos-alist)
                                     (-map #'cdr)
                                     (-map #'f-short))
                    :action '(("Switch to project" . akirak/switch-to-project-file-buffer)
                              ("Magit status" . magit-status))))))))

(general-def "C-x p" #'akirak/switch-to-project-file-buffer)

(provide 'setup-switch-buffer)
