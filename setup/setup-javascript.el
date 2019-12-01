(use-package js
  :straight (:type built-in)
  :custom
  (js-indent-level 2 "Fallback set in use-package"))

;; Prefer js-mode for LSP support
(use-package js2-mode
  :mode (("\\.js\\'" . js2-mode)))

(use-package js2-imenu-extras
  :straight js2-mode
  :after js2-mode
  :hook (js2-mode . js2-imenu-extras-mode))

(use-package js-comint
  :config
  (akirak/bind-mode-repl :keymaps '(js-mode-map typescript-mode-map)
    "" #'js-comint-repl))

(use-package add-node-modules-path
  :after js
  :hook
  ((js-mode js2-mode typescript-mode) . add-node-modules-path))

(use-package skewer-mode
  :config/el-patch
  (el-patch-defun run-skewer (&optional arg)
    "Attach a browser to Emacs for a skewer JavaScript REPL. Uses
`browse-url' to launch a browser.

With a prefix arugment (C-u), it will ask the filename of the
root document.  With two prefix arguments (C-u C-u), it will use
the contents of the current buffer as the root document."
    (interactive "p")
    (cl-case arg
      (4 (setf skewer-demo-source (read-file-name "Skewer filename: ")))
      (16 (setf skewer-demo-source (current-buffer))))
    (httpd-start)
    (el-patch-swap
      (browse-url (format "http://127.0.0.1:%d/skewer/demo" httpd-port))
      (if (fboundp 'akirak/browse-localhost)
          (akirak/browse-localhost httpd-port "/skewer/demo")
        (message "%s is undefined, so falling back to the default implementation"
                 'akirak/browse-localhost)
        (browse-url (format "http://127.0.0.1:%d/skewer/demo" httpd-port)))))
  :hook
  (js2-mode . skewer-mode)
  (css-mode . skewer-css-mode)
  (html-mode . skewer-html-mode))

(use-package jest
  :config
  (akirak/bind-mode :keymaps 'js-mode-map :package 'js
    "t" #'jest-file-dwim)
  (akirak/bind-mode :keymaps 'typescript-mode-map :package 'typescript-mode
    "t" #'jest-file-dwim))

(use-package typescript-mode
  :mode "\\.ts\\'")

(use-package tide
  :init
  (defun akirak/maybe-tide-setup ()
    (when (and (buffer-file-name)
               (not (derived-mode-p 'json-mode)))
      (tide-setup)))
  :hook
  ((js-mode typescript-mode) . akirak/maybe-tide-setup)
  (tide-mode . tide-hl-identifier-mode)
  (tide-mode . eldoc-mode)
  (tide-mode . flycheck-mode)
  :company
  (tide-mode . company-tide))

(provide 'setup-javascript)
