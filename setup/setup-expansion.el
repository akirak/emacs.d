;; Don't use abbrev-mode.
(use-package abbrev
  :disabled t
  :straight (:type built-in)
  :hook
  ((text-mode prog-mode) . abbrev-mode)
  :custom
  ;; abbrev-file-name is set externally
  (save-abbrev 'silently))

;; Use hippie-expand as an alternative to aya-open-line.
(use-package hippie-exp
  :straight (:type built-in)
  :config

  (general-def "C-o"
    (general-predicate-dispatch #'hippie-expand
      (looking-back (rx (or bol space)))
      #'akirak/org-open-line-expand))

  ;; Expand emmet templates
  ;; https://emacs.stackexchange.com/a/22527/18360
  (defun try-expand-emmet (args)
    "Expand emmet templates."
    (interactive "P")
    (when (bound-and-true-p emmet-mode)
      (emmet-exand-line args)))

  (defun akirak/org-open-line-expand (&optional n)
    (interactive "*p")
    (ignore-errors
      (if (derived-mode-p 'org-mode)
          (org-open-line (or n 1))
        (open-line (or n 1))))
    t)

  (defun akirak/hippie-yankpad-expand (arg)
    (unless yankpad-category
      (when-let (category (akirak/yankpad-guess-category))
        (yankpad-set-local-category category)))
    (when yankpad-category
      (yankpad-expand arg)))

  (setq-default hippie-expand-try-functions-list
                '(akirak/hippie-yankpad-expand
                  try-expand-emmet
                  akirak/org-open-line-expand)))

;; Load yasnippet as an infrastructure for auto-yasnippet and yankpad.
(use-package yasnippet
  :hook
  ((text-mode prog-mode) . yas-minor-mode)
  :custom
  (yas-indent-line 'fixed))

(use-package auto-yasnippet
  :commands (aya-create aya-expand)
  :config
  (setq aya-marker "~")
  :custom
  ;; I frequently use $ in templates, so something else is better
  (aya-marker-one-line "&"))

(use-package yankpad
  :config

  (defun akirak/yankpad-show-current-category ()
    (interactive)
    (let* ((category (or (buffer-local-value 'yankpad-category (current-buffer))
                         (akirak/yankpad-guess-category)))
           (major-mode-name (unless (or (eq major-mode 'org-journal-mode)
                                        (derived-mode-p 'special-mode))
                              (symbol-name major-mode)))
           (category-with-default (or category major-mode-name)))
      (with-current-buffer (or (find-buffer-visiting yankpad-file)
                               (find-file-noselect yankpad-file))
        (ignore-errors
          (unless (buffer-narrowed-p)
            (goto-char (point-min))
            (cond
             ((re-search-forward (rx-to-string `(and bol "**" (+ space)
                                                     ,category-with-default))
                                 nil t)
              (org-show-entry)
              (org-narrow-to-subtree)
              (message "Showing category \"%s\"" category-with-default))
             (category
              (error "Cannot find category \"%s\"" category))
             (t
              (re-search-forward (rx bol "*" (+ space) "Major modes"))
              (insert "\n** " major-mode-name "\n")
              (org-show-entry)
              (org-narrow-to-subtree)
              (message "Created major mode category \"%s\"" major-mode-name)))))
        (switch-to-buffer-other-window (current-buffer)))))

  ;; I do not want to use the projectile integration.
  (advice-add #'yankpad-local-category-to-projectile
              :override (lambda ()))

  (defun akirak/yankpad-helm-category-candidates ()
    (let ((data (org-ql-select yankpad-file
                  `(level ,yankpad-category-heading-level)
                  :action
                  '(let ((olp (-map #'substring-no-properties
                                    (org-get-outline-path t t))))
                     (list (-last-item olp)
                           :olp olp
                           :include (-some--> (org-entry-get nil "INCLUDE")
                                      (split-string it "|"))
                           :snippets (-map #'car (yankpad-snippets-at-point)))))))
      (cl-labels
          ((inherited-categories
            (category)
            (let* ((plist (cdr (assoc category data)))
                   (categories (plist-get plist :include)))
              (append categories
                      (-flatten-n 1 (-map #'inherited-categories categories))))))
        (-map (pcase-lambda (`(,name . ,plist))
                (cons (let ((categories (inherited-categories name)))
                        (concat (org-format-outline-path
                                 (plist-get plist :olp))
                                (if categories
                                    (format " (inherits %s)"
                                            (string-join categories ","))
                                  "")
                                "\n"
                                (string-join (plist-get plist :snippets) " / ")))
                      name))
              data))))

  (defvar akirak/yankpad-helm-category-source
    (helm-build-sync-source "Categories from yankpad-file"
      :multiline t
      :candidates #'akirak/yankpad-helm-category-candidates))

  (defun akirak/yankpad-capture-to-mode-category ()
    (interactive)
    (let* ((category (symbol-name major-mode))
           (title (read-string "Title for the template: "))
           (template (concat "*** " title " :src:\n"
                             "#+begin_src " (string-remove-suffix "-mode" category)
                             "\n"
                             (buffer-substring
                              (region-beginning)
                              (region-end))
                             "\n#+end_src\n%?"))
           (org-capture-entry
            `("y" "yankpad template"
              entry
              ,(list 'function
                     (lambda ()
                       (find-file yankpad-file)
                       (widen)
                       (goto-char (point-min))
                       (or (re-search-forward (concat "^** " (regexp-quote category)) nil t)
                           (progn
                             (re-search-forward (concat "^* Major modes"))
                             (insert "\n** " filename "\n")))))
              ,template)))
      (org-capture)))

  (defun akirak/github-workflow-file-p (&optional filename)
    (string-suffix-p "/.github/workflows/"
                     (file-name-directory (or filename
                                              (buffer-file-name)))))

  (defun akirak/yankpad-capture-file-source ()
    "Capture the region or the entire file into `yankpad-file'."
    (interactive)
    (let* ((file (or (buffer-file-name)
                     (user-error "Not in a file buffer")))
           (filename (f-filename file))
           (category (if (akirak/github-workflow-file-p file)
                         "GitHub Actions"
                       filename))
           (language (string-remove-suffix "-mode" (symbol-name major-mode)))
           (title (read-string "Title for the template: "
                               (concat
                                (if (string-equal category "GitHub Actions")
                                    "GitHub workflow"
                                  filename)
                                " for ")))
           (template (concat "*** " title " :src:\n"
                             "#+begin_src " language "\n"
                             (if (region-active-p)
                                 (buffer-substring
                                  (region-beginning)
                                  (region-end))
                               (buffer-string))
                             "\n#+end_src\n%?"))
           (org-capture-entry
            `("y" "yankpad template"
              entry
              ,(list 'function
                     (lambda ()
                       (find-file yankpad-file)
                       (widen)
                       (goto-char (point-min))
                       (or (re-search-forward (concat "^** " (regexp-quote category)) nil t)
                           (progn
                             (re-search-forward (concat "^* File names"))
                             (insert "\n** " filename "\n")))))
              ,template)))
      (org-capture)))

  (defun akirak/yankpad-guess-category ()
    "Return the default yankpad category for the file.

Determine the yankpad category based on the file name if one is
available. This is useful for per-project configuration files
sharing the common practices among different projects, such as
shell.nix."
    (let ((categories (yankpad--categories))
          (filename (buffer-file-name (or (buffer-base-buffer)
                                          (current-buffer)))))
      (or (when (and filename
                     (akirak/github-workflow-file-p filename))
            "GitHub Actions")
          (when filename
            (cl-find (file-name-nondirectory filename)
                     categories
                     :test #'string-equal))
          (when (derived-mode-p 'text-mode)
            yankpad-default-category))))

  (defun akirak/yankpad-fallback-category ()
    (when-let* ((fpath (buffer-file-name))
                (filename (file-name-nondirectory fpath))
                (mode (symbol-name major-mode))
                (categories (yankpad--categories)))
      (or (-find (lambda (category)
                   (string-match-p (wildcard-to-regexp category) filename))
                 categories)
          (cl-find mode categories :test #'string-equal))))

  (defun akirak/org-set-yankpad-category ()
    (interactive)
    (assert (derived-mode-p 'org-mode))
    (assert (not (org-before-first-heading-p)))
    (when-let (category (helm :prompt (format "Yankpad category [%s]: " yankpad-category)
                              :sources akirak/yankpad-helm-category-source))
      (org-entry-put nil "YANKPAD_CATEGORY" category)))

  (defun akirak/yankpad-get-clock-category ()
    (when (org-clocking-p)
      (org-with-point-at org-clock-marker
        (org-entry-get nil "YANKPAD_CATEGORY" t))))

  (defun akirak/yankpad-maybe-set-category (&optional arg)
    (when (or arg (not yankpad-category))
      (if-let (category (unless arg
                          (or (akirak/yankpad-guess-category)
                              (akirak/yankpad-get-clock-category))))
          (yankpad-set-local-category category)
        (if-let (category (akirak/yankpad-fallback-category))
            (progn
              (yankpad-set-local-category category)
              (message "Set the yankpad category to %s as a fallback" category))
          (user-error "Please set YANKPAD_CATEGORY in Org")))))

  (defun akirak/yankpad-insert (&optional arg)
    (interactive "P")
    (akirak/yankpad-maybe-set-category arg)
    (when yankpad-category
      (yankpad-insert)))

  (defun akirak/yankpad-map (&optional arg)
    (interactive "P")
    (akirak/yankpad-maybe-set-category arg)
    (when yankpad-category
      (yankpad-map)))

  (setq yankpad-auto-category-functions
        (list #'yankpad-major-mode-category
              (defun akirak/yankpad-project-category ()
                (and (featurep 'project)
                     (when-let* ((name (-some-> (project-current)
                                         (project-root)
                                         (f-filename))))
                       (if-let (pos (string-match "@" name))
                           (substring name 0 pos)
                         name))))))
  :custom
  (yankpad-category-heading-level 2))

(use-package emmet-mode
  :general
  (:keymaps 'web-mode-map :package 'web-mode
            "C-j" #'emmet-expand-line)
  :hook
  ((html-mode css-mode) . emmet-mode))

(general-def
  "C-x y" #'akirak/yankpad-insert)

(akirak/bind-register
  "a" 'aya-create
  "e" 'aya-expand
  "p" #'yankpad-aya-persist)

(provide 'setup-expansion)
