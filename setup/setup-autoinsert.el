(auto-insert-mode 1)
(setq auto-insert 'other
      auto-insert-query nil
      auto-insert-alist `((
                           ;; ~/.emacs.d/extras/**/*.el
                           ;; Add
                           ;; (provide 'DIRECTORY/BASENAME)
                           ;; e.g. (provide 'akirak/org-refile)
                           (,(rx "/.emacs.d/extras/" (+ anything) ".el" eos)
                            . "Extra elisp file")
                           . (> _ "\n\n"
                                "(provide '"
                                (file-relative-name
                                 (file-name-sans-extension
                                  (expand-file-name (buffer-file-name)))
                                 (expand-file-name "extras" user-emacs-directory))
                                ")"))
                          ;; setup-*.el
                          ;; Add
                          ;; (provide 'BASENAME)
                          (("/\\(init\\|my\\|setup\\)-.+\\.el\\'"
                            . "Emacs init")
                           . (> _ "\n\n"
                                "(provide '"
                                (file-name-base (buffer-file-name))
                                ")"))
                          ;; Elisp dotfiles (.*.el)
                          ;; Noop
                          ((,(rx "/." (+ (not (any "/"))) ".el" eos) . "Configuration files") . nil)
                          ;; Melpa recipes
                          ;; Insert a minimal recipe definition
                          (("melpa/recipes/.+\\'" . "Melpa recipe")
                           . (> "("
                                (file-name-nondirectory (buffer-file-name))
                                " :fetcher github :repo \""
                                akirak/github-login
                                "/"
                                _
                                "\")"))
                          ;; Fallback to "auto-insert" yasnippet template
                          (("\\.[[:alpha:]]+\\'" . "yasnippet")
                           . akirak/yas-auto-insert)))

(defun akirak/yas-auto-insert ()
  ;; Expand a snippet named \"auto-insert\" if and only if it exists
  (unless (and (eq major-mode 'emacs-lisp-mode)
               (member (file-name-base (buffer-file-name))
                       '(".dir-locals.el" "init.el")))
    (when-let ((snippet (condition-case nil
                            (yas-lookup-snippet "auto-insert")
                          (error nil))))
      (yas-expand-snippet snippet))))

(provide 'setup-autoinsert)
