;; I prefer Ivy for most situations, but Helm is better in some situations
;; especially where multiple sources are involved.

(defun akirak/build-helm-package (&optional force)
  "Build Helm package."
  (interactive "P")
  (let* ((dir (expand-file-name "straight/build/helm-core"
                                user-emacs-directory))
         (build-flag (expand-file-name ".built" dir)))
    (if (and (file-exists-p build-flag)
             (not force))
        (message "Helm has been already built")
      (cl-loop for file in (cl-sort (f-glob "*.elc" dir) #'string<)
               do (delete-file file))
      (byte-compile-file (expand-file-name "helm-source.el" dir) t)
      (cl-loop for file in (cl-sort (f-glob "*.el" dir) #'string<)
               do (byte-compile-file file))
      (call-process "touch" nil nil nil build-flag))))

(use-package helm
  :init
  ;; To prevent an error saying "Invalid function: helm-build-sync-source"
  ;; build the package.
  (akirak/build-helm-package)
  (load (expand-file-name "straight/build/helm-core/helm-core-autoloads.el"
                          user-emacs-directory))
  ;; (require 'helm-source)
  :config
  (helm-autoresize-mode 1)
  ;; Load all configuration files in lisp/my/helm/{source,action}
  (dolist (type '("source" "action"))
    (let ((dir (f-join user-emacs-directory "lisp/my/helm" type)))
      (dolist (filename (directory-files dir nil "\\.el\\'" 'nosort))
        (unless (ignore-errors (featurep (intern-sort (file-name-base filename))))
          (load (f-join dir filename) nil 'nomessage))))
    (f-files (f-join user-emacs-directory "lisp/my/helm" type)
             (lambda (filename) (string-match-p "\\.el\\'" filename))))
  :general
  ([remap yank-pop] #'helm-show-kill-ring)
  :custom
  (helm-kill-ring-max-offset 200)
  (helm-autoresize-max-height 40)
  (helm-display-function (quote pop-to-buffer)))

(use-package helm-bookmark
  :straight helm
  :config
  (add-to-list 'helm-type-bookmark-actions
               '("Terminal" . (lambda (candidate)
                                (let* ((dir (bookmark-get-filename candidate))
                                       (default-directory (if (string-suffix-p "/" dir)
                                                              dir
                                                            (error "Doesn't look like a directory bookmark: %s"
                                                                   candidate))))
                                  (vterm))))
               t))

(use-package helm-org
  :after (helm org))

(provide 'setup-helm)
