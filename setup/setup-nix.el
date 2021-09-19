(use-package nix-mode
  :mode "\\.nix\\'"
  :config
  (reformatter-define nixpkgs-fmt
    :program "nixpkgs-fmt")
  (reformatter-define nix-linter
    :program "nix-linter")
  (akirak/bind-mode :keymaps 'nix-mode-map
    "u" #'nix-update-fetch)
  (akirak/bind-mode-repl :keymaps 'nix-mode-map
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

(defun akirak/manix-source-options ()
  "Retrieve a list of possible values for --source option from the help.

This is a hack, so it may not work in the future."
  (let* ((option (->> (docopt-parse (shell-command-to-string "manix --help"))
                   (docopt-program-options)
                   (--find (equal (oref it name) "source"))))
         (description (oref option description)))
    (->> (save-match-data
           (if (string-match (rx "[possible values:"
                                 (group (+? anything))
                                 "]")
                             description)
               (split-string (match-string 1 description) ",")
             (error "Did not match on %s" description)))
      (-map #'string-trim))))

(defvar akirak/manix-query-history nil)

(defun akirak/manix-search (query &key source)
  "Search documentation for QUERY from a Nix-related SOURCE."
  (interactive (list (read-string "Search Nix documentation: "
                                  nil
                                  'akirak/manix-query-history)
                     :source (completing-read "Source: "
                                              (akirak/manix-source-options))))
  (let ((help (with-temp-buffer
                (if (zerop (apply #'process-file
                                  "manix" nil (list (current-buffer) nil) nil
                                  query
                                  (when source
                                    (list "--source" source))))
                    (buffer-string)
                  (error "manix failed with non-zero exit code")))))
    (if (string-empty-p help)
        (message "The query for \"%s\" returned no result from %s." query source)
      (with-help-window (help-buffer)
        (with-current-buffer (help-buffer)
          (insert help))))))

(defun akirak/nix-flake-attr-names (type)
  (with-temp-buffer
    (call-process "nix" nil (list t nil) nil
                  "eval" (format ".#%s.%s" type (nix-system))
                  "--apply" "builtins.attrNames" "--json")
    (goto-char (point-min))
    (ignore-errors
      (json-parse-buffer :array-type 'list))))

(defun akirak/nix-run (name &optional impure)
  ;; TODO: Make this a transient command
  (interactive (list (progn
                       (unless (file-exists-p "flake.nix")
                         (user-error "Cannot find flake.nix in the current directory"))
                       (completing-read
                        "Run Nix app/package: "
                        (-uniq (append (akirak/nix-flake-attr-names "apps")
                                       (akirak/nix-flake-attr-names "packages")))))
                     current-prefix-arg))
  (apply #'start-process
         (format "nix run %s in %s" name default-directory)
         (format "*nix run %s*" name)
         "nix" "run"
         `(,@(when impure
               '("--impure"))
           ,(format ".#%s" name))))

(provide 'setup-nix)
