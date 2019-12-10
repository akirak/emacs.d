(use-package js
  :straight (:type built-in)
  :custom
  (js-indent-level 2 "Fallback set in use-package"))

;; Prefer js-mode for LSP support
(use-package js2-mode
  :after (js json-mode)
  :mode (("\\.js\\'" . js2-mode))
  :config/el-patch
  (el-patch-defun js2-print-json-path (&optional hardcoded-array-index)
    "Print the path to the JSON value under point, and save it in the kill ring.
If HARDCODED-ARRAY-INDEX provided, array index in JSON path is replaced with it."
    (el-patch-remove (interactive "P"))
    (js2-reparse)
    (let (previous-node current-node
                        key-name
                        rlt)

      ;; The `js2-node-at-point' starts scanning from AST root node.
      ;; So there is no way to optimize it.
      (setq current-node (js2-node-at-point))

      (while (not (js2-ast-root-p current-node))
        (cond
         ;; JSON property node
         ((js2-object-prop-node-p current-node)
          (setq key-name (js2-prop-node-name (js2-object-prop-node-left current-node)))
          (if rlt (setq rlt (concat "." key-name rlt))
            (setq rlt (concat "." key-name))))

         ;; Array node
         ((or (js2-array-node-p current-node))
          (setq rlt (concat (js2-get-element-index-from-array-node previous-node
                                                                   current-node
                                                                   hardcoded-array-index)
                            rlt)))

         ;; Other nodes are ignored
         (t))

        ;; current node is archived
        (setq previous-node current-node)
        ;; Get parent node and continue the loop
        (setq current-node (js2-node-parent current-node)))
      ;; Remove side effects intended for interactive use
      (el-patch-swap
        (cond
         (rlt
          ;; Clean the final result
          (setq rlt (replace-regexp-in-string "^\\." "" rlt))
          (kill-new rlt)
          (message "%s => kill-ring" rlt))
         (t
          (message "No JSON path found!")))
        (when rlt
          ;; Clean the final result
          (setq rlt (replace-regexp-in-string "^\\." "" rlt))))
      rlt))
  :config
  ;; Based on http://blog.binchen.org/posts/use-js2-mode-as-minor-mode-to-process-json.html
  ;; Also see https://www.reddit.com/r/emacs/comments/6hdsjm/jsonnavigator_xpathwalker_jsonsnatcher_and_all/
  (defun akirak/print-json-path (&optional hardcoded-array-index)
    "Print the path to the JSON value under point, and save it in the kill ring.
If HARDCODED-ARRAY-INDEX provided, array index in JSON path is replaced with it."
    (interactive "P")
    (cond
     ((memq major-mode '(js2-mode))
      (js2-print-json-path hardcoded-array-index))
     (t
      (let* ((cur-pos (point))
             (str (buffer-substring-no-properties (point-min) (point-max))))
        (when (derived-mode-p 'json-mode)
          (setq str (format "var a=%s;" str))
          (setq cur-pos (+ cur-pos (length "var a="))))
        (with-temp-buffer
          (insert str)
          (js2-init-scanner)
          (js2-do-parse)
          (goto-char cur-pos)
          (let ((path (js2-print-json-path)))
            (when (called-interactively-p 'any)
              (message path))
            path))))))
  ;; Set eldoc-documentation-function in json-mode.
  (setq-mode-local json-mode
                   eldoc-documentation-function
                   (byte-compile
                    (lambda ()
                      (when-let ((path (akirak/print-json-path)))
                        (concat (propertize "JSON path: "
                                            'face 'font-lock-comment-face)
                                path))))))

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
