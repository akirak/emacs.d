;;;###autoload
(defun akirak/buffer-mode-name (filename)
  "Return the current major mode name without \"-mode\".

Used in source blocks."
  (with-current-buffer (find-buffer-visiting filename)
    (string-remove-suffix "-mode" (symbol-name major-mode))))

(provide 'org-capture-utils)
;;; org-capture-utils.el ends here
