(defun akirak/org-first-src-block-body (marker)
  (with-current-buffer (marker-buffer marker)
    (org-with-wide-buffer
     (goto-char marker)
     (when (re-search-forward org-babel-src-block-regexp nil t)
       (let ((end (point)))
         (org-babel-goto-src-block-head)
         (string-trim (org-element-property :value (org-element-src-block-parser end nil))))))))

(provide 'my/org/block)
