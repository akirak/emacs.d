;;; loader.el --- Load customization

;; Deprecated. Use Nix to generate ~/.emacs instead:
;; https://github.com/akirak/nixos-config/commit/418c71f0c95e7c415badf6ac3aecab8ad137a41a

(setq custom-file "~/.custom.el")
(when (file-exists-p custom-file)
  (load-file custom-file))
(load-file (expand-file-name "init.el" user-emacs-directory))

;;; loader.el ends here
