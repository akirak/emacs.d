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
  (advice-add #'yankpad-local-category-to-projectile
              :override (lambda ()))

  (setq yankpad-auto-category-functions
        (list #'yankpad-major-mode-category
              (defun akirak/yankpad-file-category ()
                (when-let (filename (buffer-file-name (or (buffer-base-buffer)
                                                          (current-buffer))))
                  (file-name-nondirectory filename)))
              (defun akirak/yankpad-org-category ()
                (and (eq major-mode 'org-mode)
                     (when-let (filename (buffer-file-name (org-base-buffer (current-buffer))))
                       (cond
                        ((member (expand-file-name filename) org-starter-known-files)
                         (file-name-base filename))
                        ((when-let (plist (org-multi-wiki-entry-file-p))
                           (symbol-name (plist-get plist :namespace))))))))
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
  "c" #'yankpad-set-category
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
