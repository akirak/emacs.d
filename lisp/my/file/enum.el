(defvar akirak/directory-contents-cache nil)

(cl-defun akirak/project-files (root &key sort)
  (let* ((attrs (file-attributes default-directory))
         (mtime (nth 5 attrs))
         (cache (assoc (cons root sort) akirak/directory-contents-cache))
         (default-directory root))
    (if (or (not (cdr cache))
            (time-less-p (cadr cache) mtime))
        (let* ((items (apply #'process-lines
                             "rg" "--files"
                             "--color=never"
                             "--iglob=!.git"
                             "--iglob=!.svn"
                             "--hidden"
                             "--one-file-system"
                             (cl-ecase sort
                               (modified '("--sortr" "modified")))))
               (cell (cons mtime items)))
          (if cache
              (setf (cdr cache) cell)
            (push (cons (cons root sort) cell) akirak/directory-contents-cache))
          items)
      (cddr cache))))

(cl-defun akirak/clear-project-file-cache (root &key sort)
  (when-let ((cache (assoc (cons root sort) akirak/directory-contents-cache)))
    (message "Clearing cache for %s..." root)
    (setcdr cache nil)))

(provide 'my/file/enum)
