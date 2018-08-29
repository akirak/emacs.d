;;; loader.el --- Load customization

(setq custom-file "~/.custom.el")
(when (file-exists-p custom-file)
  (load-file custom-file))
(load-file (expand-file-name "init.el" user-emacs-directory))

;;; loader.el ends here
