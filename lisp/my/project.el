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
                  (when root
                    (puthash dir root akirak/project-roots-cache))
                  root))
    (root root)))

(defun akirak/project-name ()
  (when-let (root (and default-directory (akirak/project-root default-directory)))
    (f-filename root)))

(defun akirak/try-init-project-root ()
  "Create a project root and return the new root."
  (-when-let ((base . level)
              (cl-find (expand-file-name default-directory)
                       (-map (lambda (entry)
                               (cons (expand-file-name (car entry))
                                     (cdr entry)))
                             magit-repository-directories)
                       :key #'car
                       :test (lambda (a b)
                               (string-prefix-p b a))))
    (let* ((root (->> (f-relative default-directory base)
                      (f-split)
                      (-take level)
                      (apply #'f-join)
                      (f-join base)))
           (root (read-directory-name "Initialize a Git repository in: " root nil t)))
      (call-process "git" nil nil nil "init" root)
      ;; Add .direnv to .gitignre
      (when (file-exists-p (expand-file-name ".envrc" root))
        (with-temp-buffer
          (insert "\n.direnv")
          (append-to-file (point-min) (point-max)
                          (expand-file-name ".gitignore" root))))
      root)))

(provide 'my/project)
