(auto-insert-mode 1)
(setq auto-insert 'other
      auto-insert-query nil
      auto-insert-alist `((("\\.[[:alpha:]]+\\'" . "yasnippet")
                           . akirak/yas-auto-insert)))

(defun akirak/yas-auto-insert ()
  (when-let ((snippet (yas-lookup-snippet "auto-insert")))
    (yas-expand-snippet snippet)))

(provide 'init-autoinsert)
