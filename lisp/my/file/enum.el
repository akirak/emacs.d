(defvar akirak/directory-contents-cache
  (make-hash-table :test #'equal :size 200))

(defvar akirak/project-files-added nil)

(defun akirak/project-files (root)
  (let* ((attrs (file-attributes default-directory))
         (mtime (nth 5 attrs))
         (cell (gethash root akirak/directory-contents-cache 'missing))
         (default-directory root))
    (if (or (eq cell 'missing)
            (time-less-p (car cell) mtime))
        (let* ((items (process-lines
                       "rg" "--files"
                       "--color=never"
                       "--iglob=!.git"
                       "--iglob=!.svn"
                       "--hidden"
                       "--one-file-system"
                       "--sortr" "modified")))
          (puthash root (cons mtime items)
                   akirak/directory-contents-cache)
          items)
      (cdr cell))))

(add-hook 'find-file-hook
          (defun akirak/set-project-files-added ()
            (set (make-variable-buffer-local 'akirak/project-files-added) t)))

(add-hook 'after-save-hook
          (defun akirak/add-project-files-cache ()
            (unless akirak/project-files-added
              (when-let (root (-some->> (project-current)
                                (project-root)
                                (expand-file-name)))
                (let ((cell (gethash root akirak/directory-contents-cache))
                      (path (file-relative-name (buffer-file-name) root)))
                  (puthash root
                           (cons (current-time)
                                 (cons path
                                       (when cell
                                         (cdr cell))))
                           akirak/directory-contents-cache)
                  (akirak/set-project-files-added)
                  (message "Added %s to cache" path))))))

(cl-defun akirak/clear-project-file-cache (root &key _sort)
  (message "Clearing cache for %s..." root)
  (remhash root akirak/directory-contents-cache))

(provide 'my/file/enum)
