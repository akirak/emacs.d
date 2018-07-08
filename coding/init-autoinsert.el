(auto-insert-mode 1)
(setq auto-insert 'other
      auto-insert-query nil
      auto-insert-alist `((("\\.[[:alpha:]]+\\'" . "yasnippet")
                           . akirak/yas-auto-insert)))

(defun akirak/yas-auto-insert ()
  ;; Expand a snippet named \"auto-insert\" if and only if it exists
  (unless (and (eq major-mode 'emacs-lisp-mode)
               (equal ".dir-locals.el"
                      (file-name-base (buffer-file-name))))
    (when-let ((snippet (condition-case nil
                            (yas-lookup-snippet "auto-insert")
                          (error nil))))
      (yas-expand-snippet snippet))))

(provide 'init-autoinsert)
