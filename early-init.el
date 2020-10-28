(let ((local-custom-file "~/local/emacs/custom.el"))
  (when (file-exists-p local-custom-file)
    (setq custom-file local-custom-file)
    (load-file custom-file)))
