(defvar akirak/compile-history-alist nil)

(defvar akirak/compile-nix-shell-args nil)

(cl-defun akirak/compile (command &rest args)
  "Wrap the command."
  (cl-check-type command string)
  (cl-check-type args list)
  (-let [(&plist :nix-shell-args :directory) args]
    (cl-check-type nix-shell-args (or null list))
    (cl-check-type directory (or null file-directory))
    (let* ((nix-shell-args (or nix-shell-args akirak/compile-nix-shell-args))
           (command (if nix-shell-args
                        (concat "nix-shell "
                                (mapconcat #'shell-quote-argument
                                           nix-shell-args " ")
                                " --command "
                                (shell-quote-argument command))
                      command))
           (directory (or directory default-directory))
           ;; The command must be dependent on the execution directory
           (entry (cons command
                        (if-let ((cell (member :directory args)))
                            (progn
                              (setcdr cell (cons directory (cddr cell)))
                              args)
                          (append `(:directory ,directory) args))))
           ;; Set the directory for compilation command
           (default-directory directory))
      ;; Make the given command appear first in the history
      (cl-delete entry akirak/compile-history-alist)
      (push entry akirak/compile-history-alist)
      (compile command))))

(defun akirak/format-compile-history-entry (cell)
  (-let* (((command . args) cell)
          ((&plist :nix-shell-args :directory) args))
    (format "%s [%s]" command directory)))

;;;; Generic definition for specific languages.

(defcustom akirak/compile-backend-alist
  '(("mix.exs"
     :helm-sources-fn akirak/helm-compile-mix-sources
     :modes (elixir-mode))
    ("package.json"
     :helm-sources-fn akirak/helm-compile-npm-sources
     :modes (javascript-mode typescript-mode web-mode))
    ("spago.dhall"
     :helm-sources-fn akirak/helm-compile-spago-sources
     :modes (purescript-mode))
    ("Makefile"
     :command counsel-compile))
  "Alist of compilation backend settings.")

(defun akirak/compile-detect-project ()
  (cl-labels
      ((match-pattern (cell &optional ignore-modes)
                      (let* ((plist (cdr cell))
                             (modes (plist-get plist :modes)))
                        (when (or ignore-modes
                                  (null modes)
                                  (apply #'derived-mode-p modes))
                          (-some--> (locate-dominating-file default-directory
                                                            (car cell))
                            (append (list :root it
                                          :filename (car cell))
                                    plist)))))
       (match-pattern-ignore-modes (cell) (match-pattern cell t)))
    (or (cl-some #'match-pattern
                 (-filter (lambda (cell) (plist-get (cdr cell) :modes))
                          akirak/compile-backend-alist))
        (cl-some #'match-pattern-ignore-modes
                 akirak/compile-backend-alist))))

(provide 'my/compile)
