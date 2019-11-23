(use-package ox-hugo :after ox
  :init
  (require 'ox-org)
  :custom
  (org-hugo-front-matter-format 'yaml))

;;; Hugo wrapper
;;;; Variables

(defvar akirak/org-hugo-base-dir nil)
(make-variable-buffer-local 'akirak/org-hugo-base-dir)
(defvar akirak/org-hugo-section nil)
(make-variable-buffer-local 'akirak/org-hugo-section)
(defvar akirak/org-hugo-keywords)
(make-variable-buffer-local 'akirak/org-hugo-keywords)

;;;; Custom variables

(defcustom akirak/hugo-default-project-directory ""
  "The root directory of my Hugo project."
  :type 'string)

;;;; Functions
;;;;; Setting up the file
(defun akirak/org-hugo-insert-buffer-header (key value)
  (org-with-wide-buffer
   (goto-char (point-min))
   (if (re-search-forward (concat (rx bol "#+")
                                  (regexp-quote key)
                                  (rx ":" (1+ space)))
                          (save-excursion
                            (re-search-forward (rx bol "*") nil t)
                            (point))
                          t)
       (progn
         (kill-line)
         (insert value))
     ;; If the first line is part of a header, go to the next line, as the line
     ;; is likely to be a title and it is important in deft-mode.
     (when (string-prefix-p "#" (thing-at-point 'line))
       (forward-line))
     (insert "#+" key ": " value "\n"))))

(defun akirak/org-hugo-get-keywords (&optional use-cache)
  (require 'org-element)
  (setq akirak/org-hugo-keywords
        (or (and use-cache akirak/org-hugo-keywords)
            (let* ((buffer-ast (org-with-wide-buffer (org-element-parse-buffer))))
              (org-element-map buffer-ast 'keyword
                (lambda (keyword)
                  (cons (org-element-property :key keyword)
                        (org-element-property :value keyword))))))))

(defun akirak/org-hugo-get-keyword (key &optional use-cache)
  (cdr (assoc key (akirak/org-hugo-get-keywords use-cache))))

(defun akirak/org-hugo-get-base-dir ()
  (or (akirak/org-hugo-get-keyword "HUGO_BASE_DIR")
      (call-interactively 'akirak/org-hugo-set-base-dir)
      akirak/org-hugo-base-dir))

(defun akirak/org-hugo-set-base-dir (dir)
  (interactive (list
                (abbreviate-file-name (read-directory-name
                                       "Set a value for HUGO_BASE_DIR: "
                                       nil nil t akirak/hugo-default-project-directory))))
  (akirak/org-hugo-insert-buffer-header "HUGO_BASE_DIR" dir)
  (setq akirak/org-hugo-base-dir dir))

(defun akirak/org-hugo-get-section (&optional use-cache)
  (or (akirak/org-hugo-get-keyword "HUGO_SECTION" use-cache)
      (call-interactively 'akirak/org-hugo-set-section)
      akirak/org-hugo-section))

(defun akirak/org-hugo-set-section (section)
  (interactive (list (completing-read
                      "Set a value for HUGO_SECTION: "
                      (directory-files
                       (expand-file-name "content" (akirak/org-hugo-get-base-dir))
                       nil "^[^.]"))))
  (akirak/org-hugo-insert-buffer-header "HUGO_SECTION" section))

(defun akirak/org-hugo-setup-file (&optional arg)
  "Set necessary file options for ox-hugo and return the settings."
  (interactive "P")
  (unless (derived-mode-p 'org-mode)
    (error "Not in org-mode"))
  (if arg
      (call-interactively 'akirak/org-hugo-set-base-dir)
    (akirak/org-hugo-get-base-dir))
  (if arg
      (call-interactively 'akirak/org-hugo-set-section)
    (akirak/org-hugo-get-section 'use-cache)))

;;;;; Finding a root subtree
(defconst akirak/org-hugo-root-regexp
  (rx bol (0+ space) ":EXPORT_FILE_NAME:"))

(defun akirak/org-hugo-find-root ()
  (interactive)
  (let (level
        export-file-name
        breadcrumb)
    (org-back-to-heading)
    (while (and (setq level (org-current-level))
                (not (setq export-file-name
                           (save-excursion
                             (and (re-search-forward akirak/org-hugo-root-regexp
                                                     (save-excursion (org-end-of-subtree))
                                                     t)
                                  (progn
                                    (beginning-of-line 1)
                                    (org-element-property :value (org-element-node-property-parser nil)))))))
                (push (cons (point) (org-heading-components)) breadcrumb)
                (> level 1))
      (org-up-heading-safe))
    (unless (and export-file-name
                 (equal export-file-name
                        (org-entry-get nil "EXPORT_FILE_NAME")))
      (setq export-file-name nil))
    (or export-file-name
        (when-let* ((cands (mapcar (lambda (params)
                                     (cons (concat (make-string (nth 1 params) ?\*)
                                                   " "
                                                   (nth 5 params))
                                           (nth 0 params)))
                                   (nreverse breadcrumb)))
                    (choice (completing-read "Choose the root of the post: " cands nil t)))
          (goto-char (cdr (assoc choice cands)))
          (when-let ((filename (read-string "Set a file name of the post: "
                                            (concat (or (org-entry-get nil "EXPORT_HUGO_SLUG")
                                                        (org-entry-get nil "CUSTOM_ID")
                                                        (akirak/org-hugo-make-slug))
                                                    ".md"))))
            (org-set-property "EXPORT_FILE_NAME" filename))
          (org-entry-get nil "EXPORT_FILE_NAME")))))

;;;;; Configuring the post
(defun akirak/org-hugo-configure-subtree-post (&optional force)
  (interactive "P")
  (unless (org-entry-get nil "EXPORT_HUGO_SLUG")
    (let ((slug (read-string "Set a slug for the post: "
                             (or (org-entry-get nil "CUSTOM_ID")
                                 (akirak/org-hugo-make-slug
                                  (nth 4 (org-heading-components)))))))
      (org-set-property "EXPORT_HUGO_SLUG" slug)))
  (when (or (not (org-entry-get-with-inheritance "EXPORT_HUGO_CATEGORIES"))
            force)
    (org-set-property "EXPORT_HUGO_CATEGORIES" nil)))

(defun akirak/org-hugo-make-slug (&optional title)
  (thread-last (or title (nth 4 (org-heading-components)))
    (downcase)
    (split-string)
    (remove-if (lambda (s) (member s '("a" "the"))))
    (pcase--flip string-join "-")
    (replace-regexp-in-string "[^[:alnum:]_-]" "")))

;;;;; Exporting
(defun akirak/org-hugo-export-subtree (&optional arg)
  (interactive "P")
  (akirak/org-hugo-setup-file)
  (save-excursion
    (when (akirak/org-hugo-find-root)
      (akirak/org-hugo-configure-subtree-post arg)
      (org-hugo-export-wim-to-md)
      (akirak/org-hugo-open-exported-file))))

(defun akirak/org-hugo-export-subtrees-in-file ()
  "Export all subtrees in the file/buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (save-excursion
      (while (re-search-forward akirak/org-hugo-root-regexp nil t)
        (org-hugo-export-wim-to-md)
        (org-end-of-subtree)))))

(define-minor-mode akirak/org-hugo-subtree-export-mode
  "Toggle auto exporting the Org file using `ox-hugo'."
  :global nil
  :lighter "Hugo Subtree"
  (if akirak/org-hugo-subtree-export-mode
      ;; When the mode is enabled
      (progn
        (add-hook 'after-save-hook #'akirak/org-hugo-export-subtrees-in-file :append :local))
    ;; When the mode is disabled
    (remove-hook 'after-save-hook #'akirak/org-hugo-export-subtrees-in-file :local)))

;;;; Opening the exported file

(defun akirak/org-hugo-get-exported-file-name ()
  "Set necessary file options for ox-hugo and return the settings."
  (interactive "P")
  (require 'f)
  (when-let* ((base-dir (akirak/org-hugo-get-base-dir))
              (section (akirak/org-hugo-get-section 'use-cache))
              (filename (or (org-entry-get nil "EXPORT_FILE_NAME")
                            (akirak/org-hugo-get-keyword "EXPORT_FILE_NAME"))))
    (f-join base-dir "content" section filename)))

(defun akirak/org-hugo-open-exported-file ()
  (interactive)
  (let* ((outfile (akirak/org-hugo-get-exported-file-name))
         (already-open (find-buffer-visiting outfile)))
    (if already-open
        (with-current-buffer already-open
          (revert-buffer t t)
          (pop-to-buffer (current-buffer)))
      (find-file-other-window outfile)
      (goto-char (point-min)))))

(provide 'setup-org-hugo)
;;; setup-org-hugo.el ends here
