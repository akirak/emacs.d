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

(provide 'my/helm/source/compile)
