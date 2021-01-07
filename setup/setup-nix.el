(use-package nix-mode
  :mode "\\.nix\\'"
  :config
  (reformatter-define nixpkgs-fmt
    :program "nixpkgs-fmt")
  (reformatter-define nix-linter
    :program "nix-linter")
  (akirak/bind-mode :keymaps 'nix-mode-map
    "u" #'nix-update-fetch)
  (akirak/bind-mode-repl :mode 'nix-mode
    "" #'nix-repl))

(use-package nix
  :straight nix-mode)

(use-package nix-buffer
  :commands (nix-buffer))

(use-package nix-shebang
  :straight nix-mode
  :functions (nix-shebang-mode)
  :config
  (add-to-list 'magic-mode-alist
               (cons (rx bol "#!/usr/bin/env nix-shell") 'nix-shebang-mode)))

(use-package helm-nixos-options
  :after (nixos-options)
  :straight (:host github :repo "travisbhartwell/nix-emacs"))

(use-package nix-update
  :commands (nix-update-fetch))

(use-package nix-sandbox)

(use-package nix-env-install)

(use-package nix-boilerplate)

(cl-defun akirak/nix-prefetch-url (url &key unpack)
  (interactive (list (string-trim (read-string "Url: "))
                     :unpack current-prefix-arg))
  (let ((err-file (make-temp-file "nix-prefetch-stderr"))
        (buffer (generate-new-buffer (format "*nix-prefetch-url %s*"
                                             url))))
    (message "Fetching %s..." url)
    (unwind-protect
        (make-process :name "nix-prefetch-url"
                      :buffer buffer
                      :stderr err-file
                      :command
                      `("nix-prefetch-url"
                        ,@(when unpack
                            '("--unpack"))
                        "--type" "sha256"
                        "--print-path"
                        ,url)
                      :sentinel
                      (lambda (proc event)
                        (when (string= event "finished\n")
                          (with-current-buffer (process-buffer proc)
                            (cl-destructuring-bind (sha256 store-path)
                                (seq-take (split-string (buffer-string) "\n")
                                          2)
                              (kill-new sha256)
                              (let ((message-log-max nil))
                                (message "Saved the store path to kill ring"))
                              (dired store-path))))))
      (delete-file err-file))))

(provide 'setup-nix)
