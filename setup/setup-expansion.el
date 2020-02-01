(use-package abbrev
  :straight (:type built-in)
  :config
  (akirak/bind-register
    "g" #'add-global-abbrev
    "l" #'edit-abbrevs)
  :hook
  ((text-mode prog-mode) . abbrev-mode)
  :custom
  ;; abbrev-file-name is set externally
  (save-abbrev 'silently))

(use-package hippie-exp
  :straight (:type built-in)
  :general
  ("C-S-f" #'hippie-expand)
  :config
  ;; Expand emmet templates
  ;; https://emacs.stackexchange.com/a/22527/18360
  (defun try-expand-emmet (args)
    "Expand emmet templates."
    (interactive "P")
    (when (bound-and-true-p emmet-mode)
      (emmet-exand-line args)))
  (setq-default hippie-expand-try-functions-list
                '(yas-hippie-try-expand
                  try-expand-emmet
                  try-complete-file-name-partially
                  try-complete-file-name)))

(use-package yasnippet
  ;; :diminish 'yas-minor-mode
  :config
  (defun akirak/visit-yas-snippet-dir ()
    (interactive)
    (find-file (car yas-snippet-dirs)))
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :after yasnippet
  :load-path "contrib/yasnippet-snippets"
  :straight (yasnippet-snippets :type built-in)
  :init
  ;; By default, the snippets in this package are loaded at package
  ;; initialization.
  ;;
  ;; This is annoying since the snippets override my own snippets
  ;; when they conflict.
  ;;
  ;; To prevent this issue, I replace `yasnippet-snippets-initialize'
  ;; with an alternative version that doesn't load the snippets.
  ;;
  ;; Instead, I call `yas-reload-all' after this package is loaded.
  ;; This works as expected since this package is loaded after
  ;; `yasnippet' package.
  (defun akirak/yasnippet-snippets-initialize-no-reload ()
    "Add the directory but don't reload the snippets."
    (add-to-list 'yas-snippet-dirs 'yasnippet-snippets-dir t))
  (advice-add 'yasnippet-snippets-initialize
              :override
              'akirak/yasnippet-snippets-initialize-no-reload)
  :config
  ;; Load all snippets. You need to make sure that all snippet
  ;; directories are added beforehand.
  (yas-reload-all))

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

  (defun akirak/yankpad-insert ()
    (interactive)
    (akirak/yankpad-ql yankpad-file '(level 2))))

(use-package ivy-yasnippet
  :commands (ivy-yasnippet))

(use-package emmet-mode
  :hook
  ((html-mode css-mode) . emmet-mode))

(provide 'setup-expansion)
