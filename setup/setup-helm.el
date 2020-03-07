;; I prefer Ivy for most situations, but Helm is better in some situations
;; especially where multiple sources are involved.

(defun akirak/build-helm-package (&optional force)
  "Build Helm package."
  (interactive "P")
  (let* ((dir "~/.emacs.d/straight/build/helm-core")
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
  (load "~/.emacs.d/straight/build/helm-core/helm-core-autoloads.el")
  ;; (require 'helm-source)
  :config
  (helm-autoresize-mode 1)
  :custom
  (helm-autoresize-max-height 40)
  (helm-display-function (quote pop-to-buffer)))

(use-package helm-org
  :after (helm org))

(provide 'setup-helm)
