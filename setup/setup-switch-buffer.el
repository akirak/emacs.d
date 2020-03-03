;;;; Project root cache
;; Based on ibuffer-project.el.
(defvar akirak/project-roots-cache (make-hash-table :test 'equal)
  "Variable to store cache of project per directory.")

(defun akirak/clear-project-cache ()
  (interactive)
  (clrhash akirak/project-roots-cache))

(defun akirak/project-root (dir)
  "Return the project root of DIR with cache enabled."
  (pcase (gethash dir akirak/project-roots-cache 'no-cached)
    ('no-cached (let* ((project (project-current nil dir))
                       (root (and project (car (project-roots project)))))
                  (puthash dir root akirak/project-roots-cache)
                  root))
    (root root)))

(defsubst akirak/buffer-derived-mode-p (buffer &rest modes)
  (declare (indent 1))
  (apply #'provided-mode-derived-p (buffer-local-value 'major-mode buffer)
         modes))

(defcustom akirak/exwm-browser-class-names
  '("Chromium" "Brave-browser" "Chromium-browser")
  "List of class names of browser windows.")

(defun akirak/reference-buffer-p (buffer)
  ;; Based on the implementation of `derived-mode-p'.
  (or (akirak/buffer-derived-mode-p buffer
        'Info-mode 'help-mode 'helpful-mode 'eww-mode)
      (and (akirak/buffer-derived-mode-p buffer
             'exwm-mode)
           (member (buffer-local-value 'exwm-class-name buffer)
                   akirak/exwm-browser-class-names))))

(defun akirak/indirect-org-buffer-p (buffer)
  (and (akirak/buffer-derived-mode-p buffer 'org-mode)
       (buffer-base-buffer buffer)))

;; TODO: Add a predicate for terminals and interactive shells

;; TODO: Add a predicate for scratch bufers

(cl-defun akirak/helm-filtered-buffer-source (name predicate &key
                                                   format-candidate
                                                   action)
  (declare (indent 1))
  (-let* (((visible-buffers windows)
           (->> (window-list)
                (-map (lambda (wnd)
                        (let ((buffer (window-buffer wnd)))
                          (when (funcall predicate buffer)
                            (list buffer wnd)))))
                (delq nil)
                (-unzip)
                (-map (lambda (list)
                        (pcase list
                          (`(,x) (list x))
                          (`(,x . ,y) (list x y))
                          (_ list))))))
          (non-visible-buffers (-difference (->> (buffer-list)
                                                 (-filter predicate))
                                            visible-buffers))
          (default-action (lambda (buf)
                            (if current-prefix-arg
                                (progn
                                  (message "Select a window")
                                  (ace-window nil)
                                  (switch-to-buffer buf))
                              (pcase windows
                                (`(,window)
                                 (select-window window)
                                 (switch-to-buffer buf))
                                ('()
                                 (pop-to-buffer buf))
                                (_
                                 (message "Select a window %s" windows)
                                 (ace-window nil)
                                 (switch-to-buffer buf)))))))
    (helm-build-sync-source name
      :candidates (-map (lambda (buf)
                          (cons (funcall (or format-candidate #'buffer-name)
                                         buf)
                                buf))
                        non-visible-buffers)
      :action (or action default-action))))

(defun akirak/switch-to-reference-buffer ()
  (interactive)
  (helm :prompt "Switch to a reference buffer: "
        :sources
        (akirak/helm-filtered-buffer-source "Reference buffers"
          #'akirak/reference-buffer-p)))

(defun akirak/switch-to-indirect-org-buffer ()
  (interactive)
  (helm :prompt "Switch to an indirect Org buffer: "
        :sources
        (akirak/helm-filtered-buffer-source "Indirect Org buffers"
          #'akirak/indirect-org-buffer-p)))

(defun akirak/switch-to-dired-buffer ()
  (interactive)
  (pcase current-prefix-arg
    ('(16) (helm :prompt "Git repositories: "
                 :sources
                 (helm-build-sync-source "magit-list-repos"
                   :candidates
                   (mapcar #'f-short (magit-list-repos))
                   :action
                   '(("Dired" . dired)
                     ("Find file" . counsel-find-file)
                     ("Term" . (lambda (dir)
                                 (let ((default-directory dir))
                                   (vterm))))))))
    ('(4) (helm :prompt "Switch to a directory in the project: "
                :sources
                (list (helm-build-sync-source "Directories in the project"
                        :candidates
                        (let ((default-directory (akirak/project-root default-directory)))
                          (->> (process-lines "fd" "-t" "d" "--color=never")
                               (-map (lambda (relative)
                                       (cons relative (f-expand relative))))))
                        :action
                        '(("Dired" . dired)
                          ("Find file" . counsel-find-file)
                          ("Term" . (lambda (dir)
                                      (let ((default-directory dir))
                                        (vterm))))))
                      (helm-build-sync-source "Ancestors of the project"
                        :candidates
                        (->> (akirak/project-root default-directory)
                             (f-expand)
                             (f-split)
                             (-inits)
                             (-butlast)
                             (cdr)
                             (-map (-partial #'apply #'f-join))
                             (-map #'f-short))
                        :action
                        '(("Dired" . dired)
                          ("Find file" . counsel-find-file)
                          ("Term" . (lambda (dir)
                                      (let ((default-directory dir))
                                        (vterm)))))))))
    ('nil (helm :prompt "Switch to a dired buffer: "
                :sources
                (list (akirak/helm-filtered-buffer-source "Dired buffers"
                        (lambda (buf)
                          (akirak/buffer-derived-mode-p buf 'dired-mode))
                        :format-candidate
                        (lambda (buf) (buffer-local-value 'default-directory buf))
                        :action
                        (lambda (buf)
                          (when current-prefix-arg
                            (ace-window nil))
                          (switch-to-buffer buf)))
                      ;; Based on `helm-source-bookmark-files&dirs' in helm-bookmark.el
                      (helm-make-source "Directory bookmarks" 'helm-source-filtered-bookmarks
                        :init (lambda ()
                                (bookmark-maybe-load-default-file)
                                (helm-init-candidates-in-buffer
                                    'global (helm-bookmark-filter-setup-alist
                                             (lambda (bookmark)
                                               (let* ((filename (bookmark-get-filename bookmark))
                                                      (isnonfile (equal filename helm-bookmark--non-file-filename)))
                                                 (and filename
                                                      (not isnonfile)
                                                      (string-suffix-p "/" filename)
                                                      (not (bookmark-get-handler bookmark))))))))))))))

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
                       (akirak/project-root default-directory))))
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
                                    (find-file (f-join root relative))))))))))

(with-eval-after-load 'helm
  (defvar akirak/git-status-source
    (helm-build-sync-source "Git status"
      :candidates (lambda () (process-lines "git" "status" "--short"))
      :persistent-action
      (lambda (status)
        (let ((file (status-file status)))
          (with-current-buffer (or (find-buffer-visiting file)
                                   (find-file-noselect file))
            (magit-diff-buffer-file))))
      :action '(("Find file" . (lambda (status)
                                 (let ((relative (status-file status)))
                                   (find-file (f-join root relative)))))))))

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

(general-def
  "C-x b" #'akirak/switch-to-project-file-buffer
  "C-x p" #'akirak/find-file-recursively
  "C-x d" #'akirak/switch-to-dired-buffer
  "C-c j" #'akirak/switch-to-indirect-org-buffer
  "C-c r" #'akirak/switch-to-reference-buffer)

(provide 'setup-switch-buffer)
