;;; init-org-hugo.el --- ox-hugo wrapper -*- lexical-binding: t -*-

(defcustom hugo-project-directory "~/personal/hugo"
  "The root directory of my Hugo project."
  :type 'string)

(defcustom akirak/hugo-categories nil
  "List of preconfigured Hugo categories."
  :group 'org-export-hugo
  :type '(repeat string))

(use-package ox-hugo :after ox
  :init
  (require 'ox-org)
  :custom
  (org-hugo-front-matter-format 'yaml))

;; Provides akirak/org-insert-buffer-header function
(require 'akirak-org-commands)
(require 'f)

(defun akirak/org-hugo-setup-buffer ()
  "Set necessary file options for ox-hugo and return the settings."
  (interactive)
  (require 'org-element)
  (unless (derived-mode-p 'org-mode)
    (error "Not in org-mode"))
  (let* ((buffer-ast (org-with-wide-buffer (org-element-parse-buffer)))
         (keywords (org-element-map buffer-ast 'keyword
                     (lambda (keyword)
                       (cons (org-element-property :key keyword)
                             (org-element-property :value keyword)))))
         ;; Check HUGO_SECTION and HUGO_BASE_DIR property
         (base-dir (or (cdr (assoc "HUGO_BASE_DIR" keywords))
                       (akirak/org-insert-buffer-header
                        "HUGO_BASE_DIR" (abbreviate-file-name
                                         (read-directory-name
                                          "Set a value for HUGO_BASE_DIR: "
                                          nil nil t hugo-project-directory)))))
         (section (or (cdr (assoc "HUGO_SECTION" keywords))
                      (akirak/org-insert-buffer-header
                       "HUGO_SECTION" (completing-read
                                       "Set a value for HUGO_SECTION: "
                                       (directory-files
                                        (expand-file-name "content" base-dir)
                                        nil "^[^.]"))))))
    `(:base-dir ,base-dir :section ,section)))

(defun akirak/ox-hugo-draft-p ()
  "Determine if the sub-tree is a draft.

According to the spec of ox-hugo, a sub-tree is a draft if its TODO state is
either TODO or DRAFT. However, because there are situations where I use NEXT to
indicate a draft to work on, I will consider a sub-tree as a published version
if and only if it has DONE state. Note that there can be ARCHIVED state with
which entry should never be published, so a general done state is not suitable
in this case."
  (not (equal "DONE" (substring-no-properties (org-get-todo-state)))))

(defun akirak/org-hugo-find-root ()
  "Move to an ancestor or self where EXPORT_FILE_NAME property is set.

If found, return the point. Otherwise, this function returns nil."
  (let ((start (point))
        level)
    (while (and (setq level (org-current-level))
                (> level 1)
                (not (org-entry-get nil "EXPORT_FILE_NAME")))
      (org-up-heading-all 1))
    (if (org-entry-get nil "EXPORT_FILE_NAME")
        (point)
      (goto-char start)
      nil)))

(defvar akirak/org-hugo-original-position nil)

(defun akirak/org-hugo-find-root-save ()
  (setq akirak/org-hugo-original-position (point))
  (akirak/org-hugo-find-root))

(defun akirak/org-export-subtree-to-hugo ()
  "Export the current subtree using ox-hugo with workarounds."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in org-mode"))
  (when (org-before-first-heading-p)
    (user-error "Before the first heading. Not exporting the subtree"))
  (save-excursion
    (akirak/org-hugo-find-root)
    (let* ((site (akirak/org-hugo-setup-buffer))
           ;; Set EXPORT_FILE_NAME property on the subtree
           (filename (or (org-entry-get nil "EXPORT_FILE_NAME")
                         (let ((filename (read-string
                                          (format "Set the export file name for the subtree \"%s\": "
                                                  (nth 4 (org-heading-components))))))
                           (org-entry-put nil "EXPORT_FILE_NAME" filename)
                           filename)))
           (outfile (f-join (plist-get site :base-dir)
                            "content"
                            (plist-get site :section)
                            filename))
           (already-open (find-buffer-visiting outfile)))
      (call-interactively 'org-hugo-export-wim-to-md)
      (if already-open
          (with-current-buffer already-open
            (revert-buffer t t)
            (pop-to-buffer (current-buffer)))
        (find-file-other-window outfile)
        (goto-char (point-min))))))

(defun akirak/org-hugo-process-tags (tags)
  "Maybe replace underscores in TAGS."
  (dolist (func org-hugo-tag-processing-functions)
    (setq tags (funcall func tags
                        (list :hugo-allow-spaces-in-tags org-hugo-prefer-hyphen-in-tags
                              :hugo-prefer-hyphen-in-tags org-hugo-prefer-hyphen-in-tags))))
  (mapconcat (lambda (tag)
               (if (string-match "[[:space:]]" tag)
                   (concat "\""
                           (replace-regexp-in-string "\"" "\\\"" tag)
                           "\"")
                 tag))
             tags " "))

(defun akirak/org-hugo-tags ()
  "Get a list of Hugo tags for the current subtree."
  (or (org-entry-get nil "EXPORT_HUGO_TAGS")
      (akirak/org-hugo-process-tags
       (cl-remove-if
        (lambda (s) (string-prefix-p "@" s))
        (let ((org-use-tag-inheritance t))
          (mapcar #'substring-no-properties
                  (org-hugo--get-tags)))))))

(defun akirak/org-hugo-categories ()
  "Get a list of Hugo categories for the current subtree."
  (or (org-entry-get nil "EXPORT_HUGO_CATEGORIES")
      (akirak/org-hugo-process-tags
       (mapcar (lambda (s) (string-remove-prefix "@" s))
               (cl-remove-if-not
                (lambda (s) (string-prefix-p "@" s))
                (seq-difference (let ((org-use-tag-inheritance t))
                                  (mapcar #'substring-no-properties
                                          (org-hugo--get-tags)))
                                org-export-exclude-tags))))))

(defhydra akirak/org-hugo-hydra (:hint nil
                                       :pre (progn
                                              (unless (derived-mode-p 'org-mode)
                                                (user-error "Not in org-mode"))
                                              (require 'ox-hugo))
                                       :post
                                       (goto-char akirak/org-hugo-original-position))
  "
Export to Hugo

Title: %(substring-no-properties (org-get-heading t t t t))
_c_ Categories: %s(akirak/org-hugo-categories)
_t_ Tags: %s(akirak/org-hugo-tags)

"
  ("t" (org-entry-put nil "EXPORT_HUGO_TAGS"
                      (read-string "Set tags: " (akirak/org-hugo-tags))))
  ("c" (org-entry-put nil "EXPORT_HUGO_CATEGORIES"
                      (let ((cat (completing-read "Set categories: "
                                                  akirak/hugo-categories
                                                  nil nil
                                                  (akirak/org-hugo-categories))))
                        (unless (member cat akirak/hugo-categories)
                          (customize-save-variable 'akirak/hugo-categories
                                                   (sort (cons cat akirak/hugo-categories)
                                                         #'string<)))
                        cat)))
  ("T" (when-let ((tag (completing-read "Tag to exclude: "
                                        (seq-difference (let ((org-use-tag-inheritance t))
                                                          (mapcar #'substring-no-properties
                                                                  (org-hugo--get-tags)))
                                                        org-export-exclude-tags)
                                        nil t)))
         (customize-save-variable 'org-export-exclude-tags
                                  (cons tag org-export-exclude-tags)))
   "Add exclude tags")
  ("C" (customize-variable-other-window 'akirak/hugo-categories)
   "Configure categories")
  ("e" akirak/org-export-subtree-to-hugo "dispatch" :exit t))

(advice-add #'akirak/org-hugo-hydra/body :before #'akirak/org-hugo-find-root-save)

(defun akirak/org-export-subtree-to-hugo-dwim (arg)
  (interactive (list current-prefix-arg))
  (if (and (save-excursion (akirak/org-hugo-find-root))
           (not arg))
      (akirak/org-export-subtree-to-hugo)
    (akirak/org-hugo-hydra/body)))

(provide 'init-org-hugo)
;;; init-org-hugo.el ends here
