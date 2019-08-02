;;;; Faces
(defface akirak/header-line-buffer-name
  '()
  "Face for the buffer name segment in a header line.")

(defface akirak/header-line-non-file-buffer-name
  '((default :inherit 'akirak/header-line-buffer-name
      :slant italic))
  "Face for non-file buffer names.")

(defface akirak/header-line-indirect-buffer-name
  '((default :inherit 'akirak/header-line-buffer-name
      :weight bold
      :slant italic))
  "Face for the buffer names of indirect buffers.")

(defface akirak/header-line-outline
  '((default :inherit font-lock-function-name-face))
  "Face for the function name or header in the header line.")

;;;; Setting the header line
(defun akirak/set-header-line ()
  (let* ((modes (let ((mode major-mode)
                      modes)
                  (catch 'ok
                    (while mode
                      (push mode modes)
                      (setq mode (get mode 'derived-mode-parent))))
                  modes))
         (fmt (cond
               ((memq 'prog-mode modes)
                (akirak/make-header-line-format))
               ((memq 'org-mode modes)
                (akirak/make-header-line-format
                 '(:eval
                   (akirak/header-line-org-outline-path))))
               ((memq 'org-agenda-mode modes)
                '((:eval (and (featurep 'all-the-icons)
                              (all-the-icons-icon-for-buffer)))
                  " "
                  (:eval (pcase org-agenda-redo-command
                           (`(org-agenda-run-series ,desc . ,_)
                            (when-let ((key (caar (cl-remove-if-not
                                                   (lambda (list) (equal (nth 1 list) desc))
                                                   org-agenda-custom-commands))))
                              (format "[%s]%s" key (car (split-string desc ":")))))
                           (x (prin1-to-string x))))))
               ((memq 'dired-mode modes)
                (progn
                  (setq dired-filter-header-line-format
                        `("  "
                          ,(if (featurep 'all-the-icons)
                               (all-the-icons-icon-for-buffer)
                             'mode-name)
                          " "
                          dired-directory
                          " "
                          (:eval (dired-filter--describe-filters))))
                  nil)))))
    (when fmt
      (setq header-line-format fmt))))

(add-hook 'after-change-major-mode-hook 'akirak/set-header-line)
(add-hook 'clone-indirect-buffer-hook 'akirak/set-header-line)

;;;; Default header line format
(defun akirak/make-header-line-format (&rest body)
  "Build a header line format with the standard set of segments."
  (let* ((filep (when buffer-file-name t))
         (base-buffer (unless filep (buffer-base-buffer)))
         (indirectp (when base-buffer t))
         (file-name (cond
                     (filep buffer-file-name)
                     (base-buffer (buffer-file-name base-buffer))))
         (project-root (when (bound-and-true-p projectile-mode)
                         (projectile-project-root)))
         (project-name (when project-root
                         (projectile-project-name)))
         (relative-name (when (and file-name project-root)
                          (file-relative-name file-name project-root)))
         (file-segment (when file-name
                         (propertize (or relative-name
                                         (file-name-nondirectory file-name))
                                     'face 'akirak/header-line-buffer-name)))
         (buffer-segment (unless filep
                           (propertize (buffer-name)
                                       'face (if indirectp
                                                 'akirak/header-line-indirect-buffer-name
                                               'akirak/header-line-non-file-buffer-name))))
         (icon (or (and (featurep 'all-the-icons)
                        (let ((file-name (buffer-file-name)))
                          (if (and file-name
                                   (bound-and-true-p polymode-mode))
                              (all-the-icons-icon-for-file file-name)
                            (all-the-icons-icon-for-buffer))))
                   mode-name)))
    `("  "
      ;; Display an icon for the mode if any
      ,icon
      " "
      ,(when project-name (format "[%s]" project-name))
      " "
      ;; If it is a file-visiting buffer, show the file name.
      ;; Otherwise, show the buffer name.
      ,file-segment
      ,buffer-segment
      "%* "
      ;; Display the statuses of the buffer
      "%n"
      (read-only-mode "<RO> ")
      ;; Display the column number if the buffer is in prog-mode
      ,(if (derived-mode-p 'prog-mode)
           "(%l,%3c) "
         " ")
      ;; Append any segments
      ,@body)))

;;;; org-mode
;; Set the header line format for org-mode with the outline path.
(defvar akirak/header-line-org-outline-path-root-level nil)
(make-variable-buffer-local 'akirak/header-line-org-outline-path-root-level)

(defun akirak/header-line-org-outline-path ()
  (let ((indirect-p (buffer-base-buffer))
        (indirect-level akirak/header-line-org-outline-path-root-level)
        (path (unless (org-before-first-heading-p)
                (org-get-outline-path t t))))
    ;; If the buffer is an indirect buffer, store the level of the root
    (when (and indirect-p (not indirect-level))
      (setq indirect-level (save-excursion
                             (goto-char (point-min))
                             (length path))
            akirak/header-line-org-outline-path-root-level indirect-level))
    ;; If the buffer is an indirect buffer, trim the root path
    (when indirect-p
      (setq path (seq-drop path indirect-level)))
    ;; Continue if and only if the path is not null
    (when path
      (org-format-outline-path
       (let* ((orig-rev (nreverse path))
              (seg-length (pcase (length orig-rev)
                            ((pred (< 4)) 8)
                            ((pred (< 2)) 12)
                            (_ 20))))
         (nreverse
          (cons (car orig-rev)
                (mapcar (lambda (s)
                          (if (> (length s) seg-length)
                              (substring s 0 seg-length)
                            s))
                        (cdr orig-rev)))))))))

(provide 'setup-header-line)
