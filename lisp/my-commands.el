;;;; Switching buffers
;; Most of these commands are bound on C-x

(use-package org-recent-headings
  :after org
  :config
  (org-recent-headings-mode 1))

(use-package my/project
  :straight (:type built-in))

(use-package my/buffer/predicate
  :straight (:type built-in))

(use-package my/dir/enum
  :straight (:type built-in))

(use-package my/helm/source/buffer
  :straight (:type built-in))

(use-package my/helm/source/file
  :straight (:type built-in))

(use-package my/helm/source/dir
  :straight (:type built-in))

(defvar akirak/directory-contents-cache nil)

(defun akirak/magit-log-file (file)
  (with-current-buffer (or (find-buffer-visiting file)
                           (find-file-noselect file))
    (magit-log-buffer-file)))

(defvar akirak/helm-project-buffer-map
  (let ((map (copy-keymap helm-map)))
    (define-key map (kbd "M-/")
      (lambda ()
        (interactive)
        (helm-run-after-quit (lambda () (akirak/find-file-recursively project)))))
    map))

(cl-defun akirak/switch-to-project-file-buffer (project)
  (interactive (list (-some-> (project-current)
                       (project-roots)
                       (car-safe))))
  (setq akirak/switch-buffer-project project)
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
            (same-project-other-buffers
             (-remove-item (current-buffer) same-project-buffers))
            (other-project-buffers (-filter #'project-bufp other-file-buffers))
            (other-projects (->> (-map #'root-of other-project-buffers)
                                 (delq nil)
                                 (-uniq))))
      (helm :prompt (format "Project %s: " project)
            :sources
            (list (cond
                   (same-project-buffers
                    (helm-build-sync-source (format "File buffers in project %s"
                                                    project)
                      :candidates (mapcar #'file-buffer-cell
                                          (or same-project-other-buffers
                                              same-project-buffers))
                      :keymap akirak/helm-project-buffer-map
                      :action akirak/switch-buffer-helm-actions))
                   (project (akirak/helm-project-file-source project)))
                  (helm-build-sync-source "File buffers in other projects"
                    :candidates (mapcar #'file-buffer-cell other-project-buffers)
                    :action akirak/switch-buffer-helm-actions)
                  (helm-build-sync-source "Other projects with open file buffers"
                    :candidates other-projects
                    :persistent-action #'kill-project-bufs
                    :action '(("Switch to project" . akirak/switch-to-project-file-buffer)
                              ("Magit status" . magit-status)))
                  (helm-build-sync-source "Recentf"
                    :candidates (-map #'f-short recentf-list)
                    :action akirak/find-file-helm-actions)
                  (helm-build-sync-source "Git repositories"
                    :candidates (->> (magit-repos-alist)
                                     (-map #'cdr)
                                     (-map #'f-short))
                    :action '(("Switch to project" . akirak/switch-to-project-file-buffer)
                              ("Magit status" . magit-status))))))))

(defvar akirak/switch-buffer-project nil
  "The root directory of the project of interest.")

(general-def
  "C-x b" #'akirak/switch-to-project-file-buffer
  "C-x p"
  (defun akirak/find-file-recursively (root)
    (interactive (list (if current-prefix-arg
                           (read-directory-name "Find files in dir: ")
                         (akirak/project-root default-directory))))
    (setq akirak/switch-buffer-project root)
    (helm :prompt (format "Browse %s: " root)
          :sources (list (akirak/helm-project-file-source root))))
  "C-x d"
  (defun akirak/switch-to-dired-buffer ()
    (interactive)
    (require 'my/helm/source/buffer)
    (require 'my/helm/source/file)
    (pcase current-prefix-arg
      ('(16) (helm :prompt "Git repositories: "
                   :sources akirak/helm-magic-list-repos-source))
      ('(4)
       (if-let (root (akirak/project-root default-directory))
           (helm :prompt "Project: "
                 :sources
                 (akirak/helm-project-root-and-ancestors-source root))
         (error "Not implemented for outside of a project")))
      ('()
       (progn
         (helm :prompt "Switch to a dired buffer: "
               :sources
               (list (akirak/helm-dired-buffer-source)
                     akirak/helm-open-buffer-directories-source
                     akirak/helm-directory-bookmark-source))))))
  "C-x j"
  (defun akirak/switch-to-org-buffer ()
    (interactive)
    (require 'helm-org-ql)
    (require 'org-recent-headings)
    (require 'my/helm/source/buffer)
    (helm :prompt "Switch to Org: "
          :sources
          (list (akirak/helm-indirect-org-buffer-source)
                helm-source-org-recent-headings
                akirak/helm-source-org-starter-known-files
                helm-source-org-ql-views)))
  "C-x '"
  (defun akirak/switch-to-reference-buffer ()
    (interactive)
    (require 'my/helm/source/buffer)
    (helm :prompt "Switch to a reference buffer: "
          :sources (akirak/helm-reference-buffer-source))))

(defun akirak/switch-to-scratch-buffer ()
  (interactive)
  (require 'my/helm/source/buffer)
  (helm :prompt "Switch to a scratch/REPL buffer: "
        :sources
        (akirak/helm-scratch-buffer-source)))

(provide 'my-commands)
