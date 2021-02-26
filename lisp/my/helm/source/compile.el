(defvar akirak/helm-compile-history-source
  (helm-make-source "Compile command history" 'helm-source-sync
    :candidates
    (lambda ()
      (-map (lambda (cell)
              (cons (akirak/format-compile-history-entry cell)
                    cell))
            akirak/compile-history-alist))
    :action
    `(("Compile"
       . ,(-applify #'akirak/compile)))))

(defconst akirak/helm-compile-command-action
  (helm-make-actions
   "Compile" #'akirak/compile
   "Compile (with args)"
   (lambda (command)
     (akirak/compile (read-string "Command: " command)))
   "Run in eshell"
   #'eshell-command))

(defclass akirak/helm-sync-compile-command-source (helm-source-sync)
  ((action :initform akirak/helm-compile-command-action)))

(defclass akirak/helm-dummy-compile-command-source (helm-source-dummy)
  ((action :initform akirak/helm-compile-command-action)))

;;;; Sources for specific languages

(cl-defun akirak/helm-npm-script-source (&key prefix)
  "Like `akirak/npm-package-json-commands', but returns candidates for a Helm source."
  (require 'my/compile/npm)
  (helm-make-source "Npm script" 'akirak/helm-sync-compile-command-source
    :candidates
    (-map (lambda (cell)
            (cons (format "%s: %s" (car cell) (cdr cell))
                  (symbol-name (car cell))))
          (akirak/npm-package-json-commands "package.json"))
    :coerce (-partial #'s-prepend (or prefix "npm run "))))

(defun akirak/helm-compile-npm-sources (&optional backend)
  (list (akirak/helm-npm-script-source)
        (helm-make-source "Basic npm commands" 'akirak/helm-sync-compile-command-source
          :candidates (akirak/npm-toplevel-commands)
          :coerce (-partial #'s-prepend "npm "))
        (helm-make-source "Any command"
            'akirak/helm-dummy-compile-command-source)))

(defun akirak/helm-compile-pnpm-sources ()
  (list (akirak/helm-npm-script-source :prefix "pnpm run ")
        ;; TODO: Add pnpm commands
        (helm-make-source "Any command"
            'akirak/helm-dummy-compile-command-source)))

(defun akirak/helm-compile-yarn-sources ()
  (list (akirak/helm-npm-script-source :prefix "yarn run ")
        ;; TODO: Add yarn commands
        (helm-make-source "Any command"
            'akirak/helm-dummy-compile-command-source)))

(defun akirak/helm-compile-mix-sources ()
  (require 'my/compile/mix)
  (list (helm-make-source "Mix commands"
            'akirak/helm-sync-compile-command-source
          :candidates
          (-map (lambda (cell)
                  (cons (format "%s %s"
                                (car cell)
                                (propertize (cdr cell)
                                            'face 'font-lock-comment-face))
                        (car cell)))
                (akirak/mix-command-alist)))))

(defun akirak/helm-compile-spago-sources ()
  (list (helm-make-source "PureScript spago commands"
            'akirak/helm-sync-compile-command-source
          :candidates
          akirak/spago-compile-command-list)))

(provide 'my/helm/source/compile)
