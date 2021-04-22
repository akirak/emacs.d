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

(cl-defstruct akirak-hoogle-entry url module package item type docs)

(cl-defmethod akirak/hoogle-format-entry ((entry akirak-hoogle-entry))
  (let ((item (akirak-hoogle-entry-item entry))
        (package (-some-> (akirak-hoogle-entry-package entry)
                   (plist-get :name)))
        (module (-some-> (akirak-hoogle-entry-module entry)
                  (plist-get :name))))
    (concat item " "
            (propertize (format "<%s> " package)
                        'face 'font-lock-constant-face)
            (propertize module 'face 'font-lock-keyword-face)
            " ")))

(cl-defmethod akirak/helm-hoogle-show-entry ((entry akirak-hoogle-entry))
  (let* ((item (akirak-hoogle-entry-item entry))
         (package (-some-> (akirak-hoogle-entry-package entry)
                    (plist-get :name)))
         (module (-some-> (akirak-hoogle-entry-module entry)
                   (plist-get :name)))
         (name (save-match-data
                 (string-match (rx bol (+ (not (any space)))) item)
                 (match-string 0 item))))
    (with-current-buffer (if helm-alive-p
                             (get-buffer-create "*hoogle tmp*")
                           (generate-new-buffer (format "*hoogle %s:%s:%s*"
                                                        package module name)))
      (erase-buffer)
      (insert (format "<a href='%s'>%s:%s</a>\n<h1>%s</h1>\n\n"
                      (akirak-hoogle-entry-url entry)
                      package module
                      name))
      (insert (akirak-hoogle-entry-docs entry))
      (shr-render-buffer (current-buffer))
      (if helm-alive-p
          (display-buffer (current-buffer))
        (catch 'exit
          (helm-run-after-exit
           (lambda ()
             (pop-to-buffer (current-buffer)
                            '(nil (inhibit-same-window . t))))))))))

(cl-defmethod akirak/hoogle-insert-entry ((entry akirak-hoogle-entry))
  (let* ((synopsis (akirak-hoogle-entry-item entry))
         (identifier (save-match-data
                       (string-match (rx bol (+ (not (any space)))) synopsis)
                       (match-string 0 synopsis)))
         (module (plist-get (akirak-hoogle-entry-module entry) :name)))
    (insert identifier)
    (save-excursion
      (haskell-navigate-imports)
      (insert (format "import %s (%s)\n" module identifier)))
    (kill-new identifier)
    (message "Saved %s in the kill ring" identifier)))

(defun akirak/haskell-hoogle-local (query)
  (interactive "sHoogle: ")
  (let* ((temp-error-file (make-temp-file "hoogle"))
         (result (-map (lambda (x)
                         (apply #'make-akirak-hoogle-entry x))
                       (unwind-protect
                           (with-temp-buffer
                             (unless (zerop (call-process
                                             "hoogle"
                                             nil (list (current-buffer) temp-error-file) nil
                                             "--json" query))
                               (with-temp-buffer
                                 (insert-file-contents temp-error-file)
                                 (error (buffer-string))))
                             (goto-char (point-min))
                             (json-parse-buffer :object-type 'plist :array-type 'list))
                         (delete-file temp-error-file)))))
    (helm :prompt query
          :sources
          (helm-build-sync-source "Hoogle"
            :candidates (-map (lambda (entry)
                                (cons (akirak/hoogle-format-entry entry) entry))
                              result)
            :action
            '(("display" . akirak/helm-hoogle-show-entry)
              ("insert" . akirak/hoogle-insert-entry))))))

(provide 'setup-haskell)
