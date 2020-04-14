(defun akirak/rename-git-repository (dir)
  (interactive (list (if current-prefix-arg
                         (read-directory-name "Git repository: " nil nil t)
                       default-directory)))
  (let ((newdir (read-directory-name (format "New name (from %s): " dir)
                                     nil nil nil dir)))
    (rename-file dir newdir)))

(provide 'my/dir/function)
