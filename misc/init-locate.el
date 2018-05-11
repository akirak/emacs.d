;;;; Database configuration

(defcustom akirak/locate-database-directory "~/.cache/my-locate"
  "Centralized directory to keep locate database files.")

(defcustom akirak/locate-roots
  `(("~/.emacs.d" "emacs.db"
     :prune-paths ("~/.emacs.d/.cache"))
    ("~/.config" "config.db"
     :prune-paths ("~/.config/chromium"))
    ,@(cl-loop for name in (directory-files "~" nil "^[^\.]")
               for path = (expand-file-name name "~")
               when (file-directory-p path)
               collect `(,path ,(concat name ".db")))
    ("/keybase" "keybase.db")
    ;; ("/etc" "etc.db")
    )
  "List of update configurations.")

(defun akirak/locate-database-file (filename)
  "Get the path to FILENAME in the database directory."
  (expand-file-name filename akirak/locate-database-directory))

(defun akirak/locate-existing-database-files ()
  "Return a list of existing database files in the configuration."
  (cl-remove-duplicates
   (cl-loop for (_ dbname . _) in akirak/locate-roots
            for dbpath = (akirak/locate-database-file dbname)
            when (file-exists-p dbpath)
            collect dbpath)
   :test 'string-equal))

(defun akirak/locate-database-arg ()
  "Generate -d option from `akirak/locate-roots'."
  (string-join (akirak/locate-existing-database-files) ":"))

;;;; updatedb

(defun akirak/locate-updatedb ()
  "Update locate databases."
  (require 'projectile)
  (make-directory akirak/locate-database-directory 'parents)
  (cl-loop for (root dbname . plist) in akirak/locate-roots
           when (file-directory-p root)
           do (apply 'start-file-process (concat "updatedb-" dbname)
                     nil "updatedb"
                     "-l" "0"
                     "-o" (akirak/locate-database-file dbname)
                     "-U" (expand-file-name root)
                     (append (cl-loop for path in (plist-get plist :prune-paths)
                                      append (list "-e" (expand-file-name path)))
                             (cl-loop for dir in (append projectile-globally-ignored-directories
                                                         projectile-globally-ignored-files)
                                      append (list "-n" dir))))))

(defun akirak/locate-rebuild-database ()
  "Delete the current database files and rebuild the databases from scratch."
  (interactive)
  (mapc #'delete-file (akirak/locate-existing-database-files))
  (akirak/locate-updatedb))

;;;;; Running updatedb with a timer

;; Update the databases every 3 hours
(run-at-time "3 hour" (* 3 3600) #'akirak/locate-updatedb)

;;;; Integration with counsel-locate

(defun akirak/counsel-locate-command (input)
  "Return a shell command based on INPUT."
  (counsel-require-program "locate")
  (format "locate -i -d '%s' --regex '%s'"
          (akirak/locate-database-arg)
          (counsel-unquote-regex-parens
           (ivy--regex input))))

(setq counsel-locate-cmd 'akirak/counsel-locate-command)

(with-eval-after-load 'ivy
  (ivy-add-actions 'counsel-locate
                   '(("j" find-file-other-window "other window"))))

;;;; Integration with helm-locate

(defun akirak/helm-locate (arg)
  (interactive "P")
  (let ((helm-locate-command (concat
                              (format "locate -i -d '%s' "
                                      (akirak/locate-database-arg))
                              " %s --regex %s")))
    (helm-locate arg)))

(provide 'init-locate)
