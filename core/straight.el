;; This library simply installs straight.el.
;; No further configuration is done.
(unless (featurep 'straight)
  (let ((bootstrap-file (concat user-emacs-directory
                                "straight/repos/straight.el/bootstrap.el"))
        (bootstrap-version 3))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage)))

;; Manage recipes in a file.
(defconst akirak/straight-default-recipes-file
  (expand-file-name "recipes.el" user-emacs-directory))

(defun akirak/straight-read-recipes-from-file (file)
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (read (current-buffer))))

(defun akirak/straight-use-recipes-from-file (file)
  (cl-labels ((register-recipe
               (recipe)
               (straight-use-package recipe 'no-clone 'no-build)))
    (mapc #'register-recipe
          (akirak/straight-read-recipes-from-file file))))
