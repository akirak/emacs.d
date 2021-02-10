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

  (setq-default hippie-expand-try-functions-list
                '(yankpad-expand
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
  (defun akirak/yankpad-ql (buffers query)
    (let ((snippets (-flatten-n 1 (org-ql-select buffers query
                                    :action
                                    '(let ((heading (car (helm-org-ql--heading 80)))
                                           (snippets (yankpad-snippets-at-point)))
                                       (if (= 1 (length snippets))
                                           (list (cons heading
                                                       (cadr snippets)))
                                         snippets))))))
      (yankpad--run-snippet (assoc (completing-read "Snippet: " snippets) snippets))))

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
              (defun akirak/yankpad-set-org-clock-category ()
                "Set `akirak/yankpad-org-clock-category' based on the Org context."
                (let* ((all-categories (yankpad--categories))
                       (choice (or (org-entry-get nil "YANKPAD_CATEGORY" t)
                                   (org-get-category))))
                  (setq akirak/yankpad-org-clock-category
                        (when (and choice
                                   (member choice all-categories))
                          choice)))))
    (add-hook 'org-clock-out-hook
              (defun akirak/yankpad-unset-org-clock-category ()
                (setq akirak/yankpad-org-clock-category nil))))

  (general-add-hook '(find-file-hook
                      after-revert-hook
                      scratch-create-buffer-hook)
                    (defun akirak/yankpad-set-category-from-org-clock ()
                      (yankpad-set-local-category (or akirak/yankpad-org-clock-category
                                                      yankpad-default-category))))

  (defun akirak/yankpad-set-category-explicitly (category)
    (yankpad-set-local-category category)
    (message "Set the yankpad category to %x" category)
    (when (and (org-clocking-p)
               (yes-or-no-p "Record this category to the clocked Org file?"))
      (let (ancestors)
        (with-current-buffer (marker-buffer org-clock-marker)
          (org-with-wide-buffer
           (goto-char org-clock-marker)
           (while (org-up-heading-safe)
             (push (propertize (thing-at-point 'line t)
                               'position (point))
                   ancestors))
           (let* ((heading (completing-read "Heading to add the category: "
                                            (nreverse ancestors)))
                  (marker (get-char-property 0 'position heading)))
             (org-entry-put point "YANKPAD_CATEGORY" category)))))))

  (defvar akirak/yankpad-helm-category-source
    (helm-build-sync-source "Categories from yankpad-file"
      :candidates #'akirak/yankpad-helm-category-candidates
      :action #'akirak/yankpad-set-category-explicitly))

  (defun akirak/yankpad-helm-set-category ()
    (interactive)
    (helm :prompt (format "Yankpad category [%s]: " yankpad-category)
          :sources akirak/yankpad-helm-category-source))

  (setq yankpad-auto-category-functions
        (list #'yankpad-major-mode-category
              (defun akirak/yankpad-file-category ()
                (when-let (filename (buffer-file-name (or (buffer-base-buffer)
                                                          (current-buffer))))
                  (file-name-nondirectory filename)))
              ;; (defun akirak/yankpad-org-file-category ()
              ;;   (and (eq major-mode 'org-mode)
              ;;        (when-let (filename (buffer-file-name (org-base-buffer (current-buffer))))
              ;;          (cond
              ;;           ((member (expand-file-name filename) org-starter-known-files)
              ;;            (file-name-base filename))
              ;;           ((when-let (plist (org-multi-wiki-entry-file-p))
              ;;              (symbol-name (plist-get plist :namespace))))))))
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

;; Remove all keybindings for abbrev-mode in C-x a
(define-key global-map (kbd "C-x a") (make-sparse-keymap))
(general-def :prefix "C-x a"
  "a" #'yankpad-append-category
  "c" #'akirak/yankpad-helm-set-category
  "p" #'yankpad-aya-persist
  "C-c" #'yankpad-capture-snippet)

(general-def
  "C-x y" #'yankpad-insert
  "C-x i" #'ivy-yasnippet)

(akirak/bind-generic
  "y" #'yankpad-map)

(akirak/bind-register
  "a" 'aya-create
  "e" 'aya-expand)

(provide 'setup-expansion)
