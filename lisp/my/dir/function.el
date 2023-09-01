(defun akirak/rename-git-repository (dir)
  (interactive (list (if current-prefix-arg
                         (read-directory-name "Git repository: " nil nil t)
                       (locate-dominating-file default-directory ".git"))))
  (let ((newdir (read-directory-name (format "New name (from %s): " dir)
                                     nil nil nil dir)))
    (rename-file dir newdir)
    newdir))

(defun akirak/rename-git-repository-and-status (dir)
  (let ((newdir (akirak/rename-git-repository)))
    (magit-status dir)))

(provide 'my/dir/function)
