(use-package sh-script
  :straight (:type built-in)
  :config
  (defun akirak/sh-override-shell ()
    ;; Since I usually don't write shell scripts specific to zsh, I
    ;; override zsh with bash. This is necessary for preventing an
    ;; error of shellcheck not enabled for zsh.
    (when (eq sh-shell 'zsh)
      (sh-set-shell "bash")))
  :hook
  (sh-set-shell . akirak/sh-override-shell)
  (sh-mode . flycheck-mode))

(provide 'setup-shell-scripts)
