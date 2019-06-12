(use-package yasnippet
  ;; :diminish 'yas-minor-mode
  :config
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

(use-package ivy-yasnippet
  :commands (ivy-yasnippet))

(provide 'setup-yasnippet)
