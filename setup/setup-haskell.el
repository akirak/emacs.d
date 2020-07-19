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
  (akirak/bind-mode :keymaps 'haskell-mode-map
    "c" #'haskell-compile
    "C" (defun akirak/hpack ()
          (interactive)
          (when-let (dir (locate-dominating-file
                          default-directory "package.yaml"))
            (let ((default-directory dir))
              (shell-command "hpack ."))))
    "f" '(:wk "format")
    "fi" '(:wk "imports")
    "fii" #'haskell-mode-format-imports
    "fis" #'haskell-sort-imports
    "fia" #'haskell-align-imports
    "fs" #'haskell-mode-stylish-buffer
    "h" #'akirak/haskell-hoogle-local
    "j" '(:wk "jump")
    "ji" #'haskell-navigate-imports)

  ;; nix-env -if https://github.com/tweag/ormolu/archive/master.tar.gz -A ormolu
  (reformatter-define ormolu
    :program "ormolu")

  :hook
  (haskell-mode . akirak/setup-haskell-mode))

(use-package dante
  :commands dante-mode
  :config
  (defun akirak/setup-dante ()
    (flycheck-mode t)
    ;; (setq-local flymake-no-changes-timeout nil)
    ;; (setq-local flymake-start-syntax-check-on-newline nil)
    ;; (setq-local flycheck-check-syntax-automatically '(save mode-enabled))
    )
  (flycheck-add-next-checker 'haskell-dante '(info . haskell-hlint))
  (akirak/bind-mode :keymaps 'dante-mode-map
    "a" #'attrap-attrap
    "," #'dante-info
    "." #'dante-type-at)
  :general
  (:keymaps 'dante-mode-map
            "C-x C-e" #'dante-eval-block)
  :hook
  (dante-mode . akirak/setup-dante))

(use-package shm
  :disabled t
  :after haskell-mode
  :hook
  (haskell-mode . structured-haskell-mode))

(use-package haskell-interactive-mode
  ;; I was unable to set up interactive-haskell-mode for snack.
  ;; Maybe I'll work on it later.
  :disabled t
  :commands interactive-haskell-mode
  :hook
  (haskell-mode . interactive-haskell-mode)
  :custom
  (haskell-process-suggest-remove-import-lines t)
  (haskell-process-auto-import-loaded-modules t)
  (haskell-process-log t))

(use-package haskell-hoogle
  :straight haskell-mode
  :after haskell-mode
  :functions (haskell-hoogle-server-live-p))

(defun akirak/hoogle-generate ()
  (interactive)
  (compilation-start "hoogle generate -i"))

(major-mode-hydra-define haskell-mode
  (:title (concat "Haskell\n"
                  (if-let (buf (dante-buffer-p))
                      (with-current-buffer buf
                        (format "Dante command line: %s\n      state: %s"
                                dante-command-line
                                dante-state))
                    "Dante: Not loaded")
                  (format "Hoogle server: %s"
                          (if (haskell-hoogle-server-live-p)
                              "Started"
                            "Not started")))
          :exit nil)
  ("Dante"
   (("dd" dante-diagnose)
    ("dr" dante-restart))
   "Hoogle"
   (("hs" haskell-hoogle-start-server)
    ("hk" haskell-hoogle-kill-server)
    ("hg" akirak/hoogle-generate))))

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
