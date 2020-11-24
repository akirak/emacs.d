;; -*- lexical-binding: t; -*-

(require 'my/helm/action/generic)
(require 'my/dir/function)
(require 'my/gitmodule/function)

(defun akirak/ensure-git-module-as-argument (f)
  "Ensure a Git submodule given as the argument to function F."
  `(lambda (submodule)
     (let-alist submodule
       (let* ((is-commonplace-repos (and (file-equal-p \.root "~/commonplace/")
                                         (string-prefix-p "repos-src/" \.path)))
              (update-args (cond
                            (is-commonplace-repos '("--depth" "1"))))
              (on-update (cond
                          (is-commonplace-repos akirak/git-bookmark-update-function)))
              (location (cond
                         (is-commonplace-repos
                          (f-join commonplace-root
                                  (concat "repos/"
                                          (string-remove-prefix
                                           "repos-src/"
                                           (f-relative \.location
                                                       commonplace-root)))))
                         (t \.location))))
         (akirak/with-ensure-git-submodule submodule
                                           update-args
                                           on-update
                                           ',f
                                           location)))))

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
             (enabled-when
              (and git (not gitmodule))
              "Rename directory and run magit status") #'akirak/rename-git-repository-and-status
              (enabled-when project "README") #'akirak/find-readme))))

(defconst akirak/helm-directory-actions-1
  (akirak/helm-make-directory-actions))

(defconst akirak/helm-git-project-actions
  (akirak/helm-make-directory-actions :git t
                                      :project t))

(defconst akirak/helm-gitmodule-actions-1
  (akirak/helm-make-directory-actions :project t
                                      :gitmodule t))

(provide 'my/helm/action/dir)
