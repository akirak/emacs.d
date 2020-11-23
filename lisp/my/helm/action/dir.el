;; -*- lexical-binding: t; -*-

(require 'my/helm/action/generic)
(require 'my/dir/function)

(cl-defun akirak/helm-make-directory-actions (&key git
                                                   project
                                                   gitmodule)
  (cl-macrolet ((enabled-when (condition str) `(if ,condition
                                                   ,str
                                                 (lambda () nil)))
                (with-dir (f) `(lambda (dir)
                                 (let ((default-directory dir))
                                   (,f))))
                (second (f) `(lambda (cell)
                               (cons (car cell)
                                     (,f (cdr cell))))))
    (mapcar (if gitmodule
                (second akirak/ensure-git-module-as-argument)
              #'identity)
            (helm-make-actions
             (enabled-when git "Magit-status") #'magit-status
             (enabled-when git "Magit log all") (with-dir magit-log-all)
             "Dired" #'dired
             (enabled-when project "Find files recursively") #'akirak/find-file-recursively
             (enabled-when (not project) "Find file") #'counsel-find-file
             "Term" (with-dir vterm)
             (enabled-when (and git (not gitmodule))
                           "Rename directory and run magit status") #'akirak/rename-git-repository-and-status))))

(defconst akirak/helm-directory-actions-1
  (akirak/helm-make-directory-actions))

(defconst akirak/helm-git-project-actions
  (akirak/helm-make-directory-actions :git t
                                      :project t))

(provide 'my/helm/action/dir)
