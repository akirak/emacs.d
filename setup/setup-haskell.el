(use-package haskell-mode
  :mode ("\\.hs\\'" . haskell-mode)
  :config
  (defun akirak/setup-haskell-mode ()
    "Turn on either lsp-mode or dante-mode."
    (interactive)
    (unless (derived-mode-p 'haskell-mode)
      (user-error "Not haskell-mode"))
    (cond
     ((and (executable-find "ghcide")
           (require 'lsp-haskell nil t))
      (lsp))
     ((require 'dante nil t)
      (dante-mode 1)))
    (haskell-auto-insert-module-template))

  ;; nix-env -if https://github.com/tweag/ormolu/archive/master.tar.gz -A ormolu
  (reformatter-define ormolu
    :program "ormolu")

  :hook
  (haskell-mode . akirak/setup-haskell-mode))

(akirak/bind-mode :keymaps 'haskell-mode-map :package 'haskell-mode
  "c" #'haskell-compile
  "g" '(:wk "generate")
  "gh" (defun akirak/hoogle-generate ()
         (interactive)
         (compilation-start "hoogle generate -i"))
  "gp" (defun akirak/hpack-generate ()
         (interactive)
         (when-let (dir (locate-dominating-file
                         default-directory "package.yaml"))
           (let ((default-directory dir))
             (shell-command "hpack ."))))
  "h" #'akirak/haskell-hoogle-local
  "i" '(:wk "imports")
  "if" #'haskell-mode-format-imports
  "is" #'haskell-sort-imports
  "ia" #'haskell-align-imports
  "ij" #'haskell-navigate-imports)

(use-package dante
  :commands dante-mode
  :config
  (defun akirak/setup-dante ()
    ;; (setq-local flymake-no-changes-timeout nil)
    ;; (setq-local flymake-start-syntax-check-on-newline nil)
    ;; (setq-local flycheck-check-syntax-automatically '(save mode-enabled))
    (flycheck-mode t))
  (flycheck-add-next-checker 'haskell-dante '(info . haskell-hlint))
  :hook
  (dante-mode . akirak/setup-dante))

(akirak/bind-mode :keymaps 'dante-mode-map :package 'dante
  "D" #'dante-diagnose
  "R" #'dante-restart
  "a" #'attrap-attrap
  "," #'dante-info
  "." #'dante-type-at)
(general-def :keymaps 'dante-mode-map :package 'dante
  "C-x C-e" #'dante-eval-block)

(use-package haskell-interactive-mode
  :straight haskell-mode
  ;; I was unable to set up interactive-haskell-mode for snack.
  ;; Maybe I'll work on it later.
  :commands interactive-haskell-mode
  :hook
  (haskell-mode . interactive-haskell-mode)
  :custom
  (haskell-process-suggest-remove-import-lines t)
  ;; I don't know which importing suggestion would be better
  (haskell-process-suggest-hoogle-imports t)
  (haskell-process-suggest-hayoo-imports t)
  (haskell-process-suggest-language-pragmas t)
  (haskell-process-auto-import-loaded-modules t)
  (haskell-process-log t))

(defun akirak/haskell-hoogle-local (query)
  (interactive "sHoogle: ")
  (cl-labels
      ((format-item
        (x)
        (let ((module (-some->> x (alist-get 'module)
                                (alist-get 'name)))
              (package (-some->> x (alist-get 'package)
                                 (alist-get 'name)))
              (item (alist-get 'item x))
              (docs (car (split-string (alist-get 'docs x) "\n"))))
          (cl-labels
              ((face (f s) (propertize s 'face f))
               (comment (s) (face 'font-lock-comment-face s)))
            (concat item
                    (if package
                        (face 'font-lock-keyword-face (format " <<%s>>" package))
                      "")
                    " "
                    (if docs (comment docs)
                      "")
                    "  "
                    (if module (face 'font-lock-constant-face module)
                      ""))))))
    (let* ((tmp-buffer (generate-new-buffer "*hoogle output*"))
           (json-object-type 'alist)
           (json-array-type 'list)
           (r (call-process "hoogle" nil (list tmp-buffer nil) nil
                            "--json" query))
           (items (when (= r 0)
                    (with-current-buffer tmp-buffer
                      (goto-char (point-min))
                      (->> (json-read)
                           (-map (lambda (x)
                                   (propertize (format-item x)
                                               'url
                                               (alist-get 'url x)))))))))
      (ivy-read "Hoogle: " items
                :history akirak/haskell-hoogle-history
                :action
                (lambda (inp)
                  (browse-url (get-char-property 0 'url inp)))))))

(provide 'setup-haskell)
