;;; setup-emacs-lisp.el --- Configuration for emacs-lisp-mode  -*- lexical-binding: t; -*-

(general-add-hook '(lisp-mode-hook
                    emacs-lisp-mode-hook)
                  '(turn-on-eldoc-mode))
(general-add-hook '(lisp-mode-hook
                    emacs-lisp-mode-hook)
                  '(rainbow-delimiters-mode-enable))

(setq-mode-local emacs-lisp-mode
                 imenu-generic-expression
                 `(("Headings" ";;[;]\\{1,8\\} \\(.*$\\)" 1)
                   (nil "^\\s-*(\\(cl-def\\(?:generic\\|ine-compiler-macro\\|m\\(?:acro\\|ethod\\)\\|subst\\|un\\)\\|def\\(?:advice\\|generic\\|ine-\\(?:advice\\|compil\\(?:ation-mode\\|er-macro\\)\\|derived-mode\\|g\\(?:\\(?:eneric\\|lobal\\(?:\\(?:ized\\)?-minor\\)\\)-mode\\)\\|inline\\|m\\(?:ethod-combination\\|inor-mode\\|odify-macro\\)\\|s\\(?:etf-expander\\|keleton\\)\\)\\|m\\(?:acro\\|ethod\\)\\|s\\(?:etf\\|ubst\\)\\|un\\*?\\)\\|ert-deftest\\)\\s-+\\(\\(?:\\sw\\|\\s_\\|\\\\.\\)+\\)" 2)
                   ("Custom vars" "^\\s-*(\\(defcustom\\)\\s-+\\(\\(?:\\sw\\|\\s_\\|\\\\.\\)+\\)" 2)
                   ("Variables" "^\\s-*(\\(def\\(?:c\\(?:onst\\(?:ant\\)?\\|ustom\\)\\|ine-symbol-macro\\|parameter\\)\\)\\s-+\\(\\(?:\\sw\\|\\s_\\|\\\\.\\)+\\)" 2)
                   ("Variables" "^\\s-*(defvar\\(?:-local\\)?\\s-+\\(\\(?:\\sw\\|\\s_\\|\\\\.\\)+\\)[[:space:]\n]+[^)]" 1)
                   ("Types" "^\\s-*(\\(cl-def\\(?:struct\\|type\\)\\|def\\(?:class\\|face\\|group\\|ine-\\(?:condition\\|error\\|widget\\)\\|package\\|struct\\|t\\(?:\\(?:hem\\|yp\\)e\\)\\)\\)\\s-+'?\\(\\(?:\\sw\\|\\s_\\|\\\\.\\)+\\)" 2)
                   ;; Only in my configuration
                   ("use-package" ,(rx "(use-package" (+ space)
                                       (group (+ (any alnum "-"))))
                    1)
                   ("org-capture" ,(rx "(org-starter-def-capture" (+ space)
                                       (+ (not space)) (+ space)
                                       (syntax string-quote)
                                       (group (+ (not (syntax string-quote)))))
                    1)))

(add-to-list 'counsel-outline-settings
             `(emacs-lisp-mode
               :outline-regexp ,(rx bol ";;" (or (+ ";")
                                                 (and (? " ") (+ "*")))
                                    (+ (any "\s\t")))
               :outline-level counsel-outline-level-emacs-lisp))

;;;; Packages
(setq-default flycheck-emacs-lisp-load-path (quote inherit))

(use-package package-lint
  :commands (package-lint-current-buffer))

(use-package buttercup
  :commands (buttercup-run-at-point
             buttercup-minor-mode)
  :config
  ;; Work around the duplicate lines issue.
  ;; See https://github.com/jorgenschaefer/emacs-buttercup/issues/181
  (advice-add #'buttercup-reporter-interactive
              :before-while
              (defun akirak/ad-bf-reporter-interactive (event arg)
                (not (eq event `spec-started))))

  (advice-add #'buttercup-colorize :filter-return #'xterm-color-filter)

  ;; There is no `buttercup-minor-mode-map'
  (akirak/bind-mode :keymaps 'emacs-lisp-mode-map
    "p" #'buttercup-run-at-point))

(add-hook 'emacs-lisp-mode-hook
          (defun akirak/turn-on-buttercup-minor-mode-conditionally ()
            (when (and (buffer-file-name)
                       (string-match-p (rx (any "-/") "test" (?  "s") ".el" eol)
                                       (buffer-file-name)))
              (buttercup-minor-mode t))))

(use-package package-build)

(use-package flycheck-package
  :commands (flycheck-package-setup))

(use-package suggest
  :commands (suggest))

(use-package el2org
  :disabled t
  :commands (el2org-generate-readme))

(use-package eros
  :config
  ;; TODO: Add an advice for lispy-eval
  (eros-mode 1))

(use-package nameless
  :commands (nameless-mode)
  :disabled t
  :general
  (:keymaps 'nameless-mode-map
            "-" 'nameless-insert-name-or-self-insert))

(use-package elx
  :straight (elx :host github :repo "emacscollective/elx"))

(use-package elinter
  :straight (:host github :repo "akirak/elinter" :branch "v4")
  :config
  (add-to-list 'auto-mode-alist
               (cons (rx "/.recipes/" (+ (not (any "/"))) eol)
                     'emacs-lisp-mode))
  :general
  (:keymaps 'emacs-lisp-mode-map
            "C-c C-l" #'elinter))

(use-package lisp-extra-font-lock)

;;;; Package editing
(defun akirak/emacs-lisp-setup-package ()
  (let* ((filename (buffer-file-name))
         (filename-nondir (and filename (file-name-nondirectory filename)))
         (dir (and filename
                   (expand-file-name
                    (abbreviate-file-name (file-name-directory filename))))))
    (when (and filename
               (or (string-prefix-p (expand-file-name "straight/" user-emacs-directory) dir)
                   (not (or (string-prefix-p (expand-file-name user-emacs-directory) dir)
                            (equal (expand-file-name "~/") dir)
                            (member filename-nondir '(".dir-locals.el"))
                            (string-match-p "-tests?\\.el\\'" filename)))))
      (flycheck-package-setup)
      (flycheck-mode 1)
      (when (fboundp 'nameless-mode)
        (nameless-mode 1)))))

(add-hook 'emacs-lisp-mode-hook #'akirak/emacs-lisp-setup-package)

(defun akirak/emacs-lisp-update-keywords ()
  (interactive)
  (goto-char (point-min))
  (when (re-search-forward (lm-get-header-re "Keywords") nil t)
    (helm :prompt "Keywords: "
          :sources
          (helm-build-sync-source "finder-known-keywords"
            :candidates (-map (lambda (cell)
                                (cons (format "%s: %s"
                                              (car cell)
                                              (cdr cell))
                                      (car cell)))
                              finder-known-keywords)
            :action
            (lambda (_)
              (save-excursion
                (delete-region (point) (line-end-position))
                (insert " " (mapconcat #'symbol-name
                                    (helm-marked-candidates)
                                    " "))))))))

(defun akirak/emacs-lisp-package-dependencies ()
  (let* ((src (buffer-substring-no-properties (point-min) (point-max)))
         (requires (->> (s-match-strings-all (rx bol "(require '"
                                                 (group (+ (any "-" alnum)))
                                                 ")")
                                             src)
                        (--map (nth 1 it))))
         (exts (->> (s-match-strings-all (rx bol "(declare-function"
                                             (+ space)
                                             (?  "#") "'" (+ (any "-" alnum))
                                             (+ space)
                                             "\""
                                             (group (+ (any "-:" alnum)))
                                             "\"")
                                         src)
                    (--map (nth 1 it))
                    (--map (string-remove-prefix "ext:" it))))
         packages)
    (cl-labels
        ((add-version
          (name)
          (let* ((file (file-truename (find-library-name (symbol-name name))))
                 (existing (find-buffer-visiting file))
                 (buffer (or existing
                             (find-file-noselect file))))
            (prog1 (cons name (s-match (rx bol
                                           (+ (any digit))
                                           (optional (and "."
                                                          (+ (any digit)))))
                                       (or (with-current-buffer buffer
                                             (lm-header "Version"))
                                           "0")))
              (unless existing
                (kill-buffer buffer))))))
      ;; Retrieve a list of packages using straight.el.
      ;;
      ;; The result is stored in packages
      (maphash (lambda (package _recipe)
                 (let ((sym (intern package)))
                   (unless (gethash sym straight--cached-built-in-packages)
                     (push sym packages))))
               straight--recipe-cache)
      (->> (-uniq (append requires exts))
           (-map #'intern)
           (cl-intersection packages)
           (-map #'add-version)))))

(defun akirak/emacs-lisp-update-package-requires ()
  (interactive)
  (goto-char (point-min))
  (when (re-search-forward (lm-get-header-re "Package-Requires") nil t)
    (let* ((initial (point))
           (orig (read (current-buffer)))
           (new (cons (assoc 'emacs orig)
                      (akirak/emacs-lisp-package-dependencies))))
      (delete-region initial (line-end-position))
      (insert "(" (mapconcat #'prin1-to-string
                             new " ")
              ")"))))

;; Stolen from https://github.com/alphapapa/emacs-package-dev-handbook#bench-macro
;; Originally from https://phillord.github.io/m-buffer-el/#sec-5-1-2
(cl-defmacro bench (&optional (times 100000) &rest body)
  "Call `benchmark-run-compiled' on BODY with TIMES iterations, returning list suitable for Org source block evaluation.
Garbage is collected before calling `benchmark-run-compiled' to
avoid counting existing garbage which needs collection."
  (declare (indent defun))
  `(progn
     (garbage-collect)
     (list '("Total runtime" "# of GCs" "Total GC runtime")
           'hline
           (benchmark-run-compiled ,times
             (progn
               ,@body)))))

;;;; Commands
(defun akirak/straight-pull-package-project (name)
  "Pull the package recipe for the current project."
  (interactive
   (list (completing-read "Package: "
                          (--map (f-filename it)
                                 (f-directories
                                  (expand-file-name "straight/repos"
                                                    user-emacs-directory)))
                          nil t
                          (when-let (root (project-roots (project-current)))
                            (f-filename root)))))
  (straight-pull-package (intern name))
  (when (yes-or-no-p "Rebuild the package? ")
    (straight-rebuild-package name))
  (when (yes-or-no-p "Reload it? ")
    (load-library name)))

(defun akirak/eval-buffer-or-load-file ()
  (interactive)
  (when (eq major-mode 'emacs-lisp-mode)
    (if buffer-file-name
        (load-file buffer-file-name)
      (eval-buffer))))

(akirak/bind-mode :keymaps 'emacs-lisp-mode
  "e" #'akirak/eval-buffer-or-load-file)

(provide 'setup-emacs-lisp)
