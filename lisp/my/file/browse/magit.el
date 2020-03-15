(defun akirak/magit-log-file (file)
  "Browse FILE using `magit-log-buffer-file'."
  (with-current-buffer (or (find-buffer-visiting file)
                           (find-file-noselect file))
    (magit-log-buffer-file)))

(provide 'my/file/browse/magit)
