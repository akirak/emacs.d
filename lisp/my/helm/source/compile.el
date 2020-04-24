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

(provide 'my/helm/source/compile)
