(defcustom akirak/org-wiki-dirs
  '((technical "~/lib/notes/wiki/"))
  "List of directories containing wiki entries."
  :type '(repeat (list (symbol :tag "ID")
                       directory)))

(defvar akirak/org-wiki-dir nil
  "Current wiki directory.")

(defcustom akirak/org-wiki-default-dir nil
  "Default wiki directory."
  :type 'directory
  :set (lambda (sym value)
         (set sym value)
         (unless akirak/org-wiki-dir
           (setq akirak/org-wiki-dir value))))

(defun akirak/org-wiki-directory (id)
  (car-safe (alist-get id akirak/org-wiki-dirs)))

(defun akirak/org-wiki-select-directory (&optional prompt)
  (intern (completing-read (or prompt "Wiki: ")
                           (mapcar #'car akirak/org-wiki-dirs))))

(defun akirak/org-wiki-switch (id)
  (interactive (list (akirak/org-wiki-select-directory)))
  (when-let ((dir (akirak/org-wiki-directory id)))
    (message "Wiki directory set to %s" (setq akirak/org-wiki-dir dir))))

(defun akirak/org-wiki-escape-file-name (name)
  (--> name
       (s-replace-regexp (rx (not (any space alnum "-"))) "" it)
       (split-string it " ")
       (mapcar #'s-upper-camel-case it)
       (string-join it)))

(defun akirak/org-wiki-template (heading)
  (concat "* " heading "\n:PROPERTIES:\n"
          ":CREATED_TIME: " (org-format-time-string (org-time-stamp-format 'long 'inactive))
          "\n:END:\n"))

(defun akirak/wiki-visit-entry (heading &optional dir-id)
  (let* ((dir (if dir-id (akirak/org-wiki-directory dir-id) akirak/org-wiki-dir))
         (filename (concat (akirak/org-wiki-escape-file-name heading) ".org"))
         (fpath (expand-file-name filename dir)))
    (unless (and dir (file-directory-p dir))
      (user-error "Wiki directory is nil or missing: %s" dir))
    (unless (file-exists-p fpath)
      (with-temp-buffer
        (insert (akirak/org-wiki-template heading))
        (write-file fpath)))
    (find-file-other-window fpath)))

(defun akirak/org-wiki-entry-files (dir)
  (directory-files dir t "\\.org\\'"))

(defun akirak/helm-org-ql-wiki (&optional dir-id)
  (interactive (list (when current-prefix-arg
                       (akirak/org-wiki-select-directory))))
  ;; Based on the implementation of helm-org-ql.
  (let ((boolean 'and)
        (helm-input-idle-delay helm-org-ql-input-idle-delay)
        (files (akirak/org-wiki-entry-files (if dir-id
                                                (akirak/org-wiki-directory dir-id)
                                              akirak/org-wiki-dir))))
    (helm :prompt (format "Query (boolean %s): " (-> boolean symbol-name upcase))
          :sources
          (list (helm-org-ql-source files
                                    :name (format "Wiki (%s)" (or dir-id "default")))
                (helm-build-dummy-source "New entry"
                  :action `(lambda (inp)
                             (akirak/wiki-visit-entry inp ,dir-id)))))))

;; TODO: Add a custom link type for the wiki

;; TODO: Make org-store-link require CUSTOM_ID in wikis

(provide 'setup-org-wiki)
