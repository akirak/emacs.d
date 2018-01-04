(tool-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)
(setq inhibit-startup-screen t
      ring-bell-function 'ignore)

;; Set the frame title to a relative path of your home directory. Useful with emacs-pg
(let ((relpath (file-relative-name (expand-file-name "~")
                                   (expand-file-name (concat "~" user-login-name)))))
  (add-to-list 'default-frame-alist `(title . ,relpath)))

(use-package dracula-theme :straight t)
