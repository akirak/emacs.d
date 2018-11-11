(use-package all-the-icons)

(defface akirak/header-line-buffer-name
  '()
  "Face for the buffer name segment in a header line.")

(defface akirak/header-line-non-file-buffer-name
  '((default :inherit 'italic))
  "Face for non-file buffer names.")

(defface akirak/header-line-indirect-buffer-name
  '((default :inherit 'bolditalic))
  "Face for the buffer names of indirect buffers.")

(defface akirak/header-line-outline
  '((default :inherit font-lock-function-name-face))
  "Face for the function name or header in the header line.")

(defun akirak/header-line-buffer-segment ()
  "Generate the buffer segment in the header line."
  (propertize
   (if buffer-file-name
       ;; If the file is inside a project, show the
       ;; relative file path from the root.
       (concat (if-let ((root (projectile-project-root)))
                   (file-relative-name buffer-file-name root)
                 (file-name-nondirectory buffer-file-name))
               (when (buffer-modified-p) "*"))
     (propertize (buffer-name)
                 'face
                 (if (buffer-base-buffer)
                     'akirak/header-line-indirect-buffer-name
                   'akirak/header-line-non-file-buffer-name)))
   'face 'akirak/header-line-buffer-name))

(defun akirak/make-header-line-format (&rest body)
  "Build a header line format with the standard set of segments."
  `("  "
    ;; Display an icon for the mode if any
    (:eval (or (and (featurep 'all-the-icons)
                    (all-the-icons-icon-for-buffer))
               mode-name))
    " "
    ;; If it is a file-visiting buffer, show the file name.
    ;; Otherwise, show the buffer name.
    (:eval (akirak/header-line-buffer-segment))
    ": "
    ;; Display the statuses of the buffer
    (:eval (when (buffer-narrowed-p) "<N>"))
    (read-only-mode "<RO>")
    ;; Display the column number if the buffer is in prog-mode
    ,(if (derived-mode-p 'prog-mode)
         "%3c "
       " ")
    ;; Append any segments
    ,@body))

;;;; Default header line with which-function

;; The default header line format with which-function.
(which-function-mode 1)

(defun akirak/set-default-header-line ()
  "Set the default header line with which-function."
  (unless header-line-format
    (setq header-line-format (akirak/make-header-line-format
                              ;; Omit which-func if the buffer is indirect
                              (unless (buffer-base-buffer)
                                `(which-func-mode
                                  (:eval
                                   (propertize ,(cadr which-func-current)
                                               'face 'akirak/header-line-outline))))))))

;;;;; Setting the default header line

;; As I don't know how to disable the header line in the which-key window
;; (I tried `which-key-init-buffer-hook', but it didn't work),
;; I turn on the default header line only in certain groups of buffers
;; rather than setting it using `setq-default'.

;; Display a header line whenever you visit a file
(add-hook 'find-file-hook #'akirak/set-default-header-line)

;; Display a header line in prog-mode even if you are not visiting
;; a file, e.g. in a scratch buffer
(add-hook 'prog-mode-hook #'akirak/set-default-header-line)

;; Magit buffers should display their names
(add-hook 'magit-mode-hook #'akirak/set-default-header-line)

;; Help buffers should display their names
(add-hook 'help-mode-hook #'akirak/set-default-header-line)
(add-hook 'helpful-mode-hook #'akirak/set-default-header-line)

;; Interactive shells
(add-hook 'eshell-mode-hook #'akirak/set-default-header-line)
(add-hook 'term-mode-hook #'akirak/set-default-header-line)
(add-hook 'comint-mode-hook #'akirak/set-default-header-line)

;;;; Header line formats for specific modes
;;;;; org-mode
;; Set the header line format for org-mode with the outline path.
(setq-mode-local org-mode
                 header-line-format
                 (akirak/make-header-line-format
                  '(:eval
                    (unless (or (buffer-base-buffer)
                                (org-before-first-heading-p))
                      (org-format-outline-path
                       (let* ((orig-rev (nreverse (org-get-outline-path t t)))
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
                                        (cdr orig-rev))))))))))

;;;;; org-agenda-mode
(defun akirak/set-org-agenda-header-line ()
  (setq header-line-format
        '((:eval (and (featurep 'all-the-icons)
                      (all-the-icons-icon-for-buffer)))
          " "
          (:eval (pcase org-agenda-redo-command
                   (`(org-agenda-run-series ,desc . ,_)
                    (when-let ((key (caar (cl-remove-if-not
                                           (lambda (list) (equal (nth 1 list) desc))
                                           org-agenda-custom-commands))))
                      (format "[%s]%s" key (car (split-string desc ":")))))
                   (x (prin1-to-string x)))))))

(add-hook 'org-agenda-mode-hook #'akirak/set-org-agenda-header-line)

;;;;; dired with dired-filter
(setq dired-filter-header-line-format
      `("  "
        (:eval (or (and (featurep 'all-the-icons)
                        (all-the-icons-icon-for-buffer))
                   mode-name))
        " "
        dired-directory
        " "
        (:eval (dired-filter--describe-filters))))

(provide 'init-header-line)
