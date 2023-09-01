;; Largely based on https://github.com/purcell/emacs.d/blob/master/lisp/init-purescript.el

(defconst akirak/spago-compile-command-list
  '("spago build"
    "spago test"
    "spago install"
    "spago run"
    "spago bundle-app"
    "spago bundle-module"
    "spago docs"))

(use-package purescript-mode
  :mode "\\.purs\\'"
  :config
  (akirak/bind-mode :keymaps 'purescript-mode-map
    "<tab>" #'purescript-indent-cycle
    "g" '(:wk "navigate")
    "gi" #'purescript-navigate-imports
    "," (defrepeater 'purescript-move-nested-left)
    "." (defrepeater 'purescript-move-nested-right))
  :hook
  (purescript-mode . turn-on-purescript-indentation)
  (purescript-mode . turn-on-purescript-decl-scan))

(use-package psc-ide
  :after purescript-mode
  :config
  (defun akirak/setup-psc-ide-mode ()
    (flycheck-mode t)
    ;; (setq-local flycheck-check-syntax-automatically '(save mode-enabled))
    (turn-on-purescript-indentation))
  (akirak/bind-mode :keymaps 'psc-ide-mode-map
    "s" '(:wk "server")
    "ss" #'psc-ide-server-start
    "sq" #'psc-ide-server-quit
    "l" '(:wk "load")
    "la" #'psc-ide-load-all
    "ll" #'psc-ide-load-module
    "f" '(:wk "fix")
    "fa" #'psc-ide-flycheck-insert-suggestion
    "fc" #'psc-ide-add-clause
    "fs" #'psc-ide-case-split
    "fi" #'psc-ide-add-import
    "b" #'psc-ide-rebuild)
  :hook
  (purescript-mode . psc-ide-mode)
  (psc-ide-mode . akirak/setup-psc-ide-mode)
  :custom
  (psc-ide-add-import-on-completion t))

(use-package psci
  :hook
  (purescript-mode . inferior-psci-mode))

(defsubst akirak/spago-root (&optional dir)
  (locate-dominating-file (or dir default-directory) "spago.dhall"))

(defun akirak/spago-module-docs (&optional root)
  (let ((dir (f-join (or root (akirak/spago-root)) "generated-docs" "html")))
    (if (f-directory-p dir)
        (f-files dir (-partial #'string-suffix-p ".html"))
      (error "Documentation has not been generated yet"))))

(defun akirak/spago-browse-module-docs (&optional force-generate)
  (interactive "P")
  (let ((root (akirak/spago-root)))
    (unless root
      (user-error "Not a spago project"))
    (when force-generate
      (let ((default-directory root))
        (async-start-process "spago" "spago" #'akirak/spago-browse-module
                             "docs")))
    (if-let (files (ignore-errors (akirak/spago-module-docs root)))
        (helm :prompt "PureScript module: "
              :sources
              (helm-build-sync-source "Module in the project"
                :candidates (-map (lambda (file) (cons (f-base file) file))
                                  files)
                :action #'akirak/browse-url-for-referencing))
      (when (yes-or-no-p "Generate documentation?")
        (akirak/spago-browse-module t)))))

(defvar akirak/spago-docs-cache nil)

(defun akirak/spago-project-entries (&optional root)
  (let* ((root (or root
                   (akirak/spago-root)
                   (user-error "No spago root")))
         (cache akirak/spago-docs-cache)
         (project-cache (alist-get root cache
                                   nil nil #'file-equal-p)))
    (unless project-cache
      (setq project-cache (make-hash-table :test #'equal
                                           :size 10000))
      (push (cons root project-cache) akirak/spago-docs-cache))
    (->> (f-join (or root (akirak/spago-root)) "output")
         (f-directories)
         (-map (lambda (subdir) (f-join subdir "docs.json")))
         (-filter #'f-exists-p)
         (-map (lambda (file)
                 (or (cl-gethash file project-cache)
                     (let* ((json-object-type 'alist)
                            (json-array-type 'list)
                            (object (with-temp-buffer
                                      (insert-file-contents file)
                                      (goto-char (point-min))
                                      (json-read-object))))
                       (cl-puthash file object project-cache)
                       object)))))))

(defun akirak/format-spago-docs-entries ()
  (cl-labels
      ((format-decl-info
        (decl)
        (let* ((name (alist-get 'title decl))
               (info (alist-get 'info decl))
               (declType (alist-get 'declType info)))
          (concat (if declType
                      (format " [%s]" declType)
                    "")
                  " :: "
                  (or (format-type (alist-get 'type info))
                      "(type unknown)"))))
       (format-type
        (type-info)
        (let ((tag (alist-get 'tag type-info))
              (contents (alist-get 'contents type-info)))
          (pcase tag
            ("TypeApp" (string-join (-map #'format-type contents)
                                    " -> "))
            ("TypeConstructor" (format "%s" (nth 1 contents)))
            (_ (format "(%s)" tag))))))
    (->> (akirak/spago-project-entries)
         (-map (lambda (module)
                 (let ((module-name (alist-get 'name module)))
                   (cl-labels
                       ((scan-decl
                         (decl &rest path)
                         (let* ((name (alist-get 'title decl))
                                (self (cons (concat (string-join
                                                     `(,module-name
                                                       ,@path
                                                       ,name)
                                                     ".")
                                                    (format-decl-info decl))
                                            (list module-name
                                                  path
                                                  decl))))
                           (cons self
                                 (->> (alist-get 'children decl)
                                      (-map (lambda (child)
                                              (apply #'scan-decl
                                                     child
                                                     (append path
                                                             (list name)))))
                                      (-flatten-n 1))))))
                     (->> (alist-get 'declarations module)
                          (-map #'scan-decl)
                          (-flatten-n 1))))))
         (-flatten-n 1))))

(defun akirak/spago-browse-docs ()
  (interactive)
  (unless (akirak/spago-root)
    (user-error "not in spago root"))
  (cl-labels
      ((browse-source
        (data)
        (let* ((span (alist-get 'sourceSpan (nth 2 data)))
               (start (alist-get 'start span))
               (end (alist-get 'end span))
               (file (f-join (akirak/spago-root)
                             (alist-get 'name span))))
          (find-file file)
          (widen)
          (let ((begp (save-excursion
                        (goto-line (nth 0 start))
                        (move-to-column (1- (nth 1 start)))
                        (point))))
            (when (and begp (integerp begp) (> begp 0))
              (goto-char begp)
              (let ((endp (save-excursion
                            (end-of-defun)
                            (point))))
                (when (> endp begp)
                  (narrow-to-region begp endp))))))))
    (helm :prompt "PureScript docs: "
          :sources
          (helm-build-sync-source "Current project (via spago): "
            :candidates #'akirak/format-spago-docs-entries
            :persistent-action #'browse-source
            :action #'princ))))

(provide 'setup-purescript)
