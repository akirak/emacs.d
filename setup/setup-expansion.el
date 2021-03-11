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
  :config
  (let ((dir "~/god/emacs/yasnippet/snippets/"))
    (when (file-directory-p dir)
      (add-to-list 'yas-snippet-dirs dir)))
  :custom
  (yas-indent-line 'fixed))

(use-package auto-yasnippet
  :commands (aya-create aya-expand))

(use-package yankpad
  :config

  ;; I do not want to use the projectile integration.
  (advice-add #'yankpad-local-category-to-projectile
              :override (lambda ()))

  (defun akirak/yankpad-get-category-data ()
    (let ((olp (-map #'substring-no-properties
                     (org-get-outline-path t t))))
      (list (-last-item olp)
            :olp olp
            :include (-some--> (org-entry-get nil "INCLUDE")
                       (split-string it "|")))))

  (defun akirak/yankpad-helm-category-candidates ()
    (let ((data (org-ql-select yankpad-file
                  `(level ,yankpad-category-heading-level)
                  :action #'akirak/yankpad-get-category-data)))
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
                                  "")))
                      name))
              data))))

  (defvar akirak/yankpad-org-clock-category nil)

  (with-eval-after-load 'org-clock
    (add-hook 'org-clock-in-hook
              (defun akirak/yankpad-use-org-clock-category ()
                "Set `akirak/yankpad-org-clock-category' based on the Org context."
                (when (require 'yankpad nil t)
                  (setq akirak/yankpad-org-clock-category
                        (or (cl-find (or (org-entry-get nil "YANKPAD_CATEGORY" t)
                                         (org-get-category))
                                     (yankpad--categories)
                                     :test #'string-equal)
                            yankpad-default-category)))))
    (add-hook 'org-clock-out-hook
              (defun akirak/yankpad-unset-org-clock-category ()
                (setq akirak/yankpad-org-clock-category nil))))

  (defun akirak/yankpad-set-clock-category (category)
    (setq akirak/yankpad-org-clock-category category)
    (when (and (org-clocking-p)
               (yes-or-no-p "Record this category to the clocked Org file?"))
      (let (ancestors)
        (org-with-point-at org-clock-marker
          (cl-labels
              ((add-heading ()
                            (push (propertize (thing-at-point 'line t)
                                              'position (point))
                                  ancestors)))
            (save-excursion
              (add-heading)
              (while (org-up-heading-safe)
                (add-heading)))))
        (let* ((heading (completing-read "Heading to add the category: "
                                         (nreverse ancestors)))
               (marker (get-char-property 0 'position heading)))
          (org-entry-put point "YANKPAD_CATEGORY" category)))))

  (defvar akirak/yankpad-helm-category-source
    (helm-build-sync-source "Categories from yankpad-file"
      :candidates #'akirak/yankpad-helm-category-candidates
      :action #'akirak/yankpad-set-clock-category))

  (defun akirak/yankpad-helm-set-category ()
    (interactive)
    (helm :prompt (format "Yankpad category [%s]: " yankpad-category)
          :sources akirak/yankpad-helm-category-source))

  (defun akirak/yankpad-guess-category ()
    "Return the default yankpad category for the file.

Determine the yankpad category based on the file name if one is
available. This is useful for per-project configuration files
sharing the common practices among different projects, such as
shell.nix."
    (let* ((categories (yankpad--categories))
           (filename (buffer-file-name (or (buffer-base-buffer)
                                           (current-buffer)))))
      (or (when filename
            (or (cl-find (file-name-nondirectory filename)
                         categories)
                (when (eq major-mode 'org-mode)
                  (cl-find (file-name-base filename)
                           categories))))
          (when (derived-mode-p 'text-mode)
            yankpad-default-category))))

  (defun akirak/yankpad-insert (&optional arg)
    (interactive "P")
    (unless yankpad-category
      (if-let (category (and (not arg)
                             (akirak/yankpad-guess-category)))
          (yankpad-set-local-category category)
        (akirak/yankpad-helm-set-category)
        (yankpad-set-local-category akirak/yankpad-org-clock-category)))
    (when yankpad-category
      (yankpad-insert)))

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

;; Use ivy-yasnippet as an alternative for insert-file-contents.
(use-package ivy-yasnippet
  :commands (ivy-yasnippet))

(use-package emmet-mode
  :general
  (:keymaps 'web-mode-map :package 'web-mode
            "C-j" #'emmet-expand-line)
  :hook
  ((html-mode css-mode) . emmet-mode))

(general-def
  "C-x y" #'akirak/yankpad-insert
  "C-x i" #'ivy-yasnippet)

(akirak/bind-generic
  "y" #'yankpad-map)

(akirak/bind-register
  "a" 'aya-create
  "e" 'aya-expand
  "p" #'yankpad-aya-persist)

(provide 'setup-expansion)
