;;; Project root cache -*- lexical-binding: t -*-

(require 'project)

;; Based on ibuffer-project.el.
(defvar akirak/project-roots-cache (make-hash-table :test 'equal)
  "Variable to store cache of project per directory.")

(defun akirak/clear-project-cache ()
  (interactive)
  (clrhash akirak/project-roots-cache))

(defun akirak/project-root (dir)
  "Return the project root of DIR with cache enabled."
  (pcase (gethash dir akirak/project-roots-cache 'no-cached)
    ('no-cached (let* ((project (project-current nil dir))
                       (root (and project (car (project-roots project)))))
                  (puthash dir root akirak/project-roots-cache)
                  root))
    (root root)))

(defun akirak/project-name ()
  (when-let (root (and default-directory (akirak/project-root default-directory)))
    (f-filename root)))

(provide 'my/project)
