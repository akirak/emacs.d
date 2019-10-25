(use-package sh-script
  :straight (:type built-in)
  :config
  (defun akirak/setup-flycheck-shellcheck ()
    (interactive)
    (setq flycheck-sh-shellcheck-executable "shellcheck")
    (unless (executable-find "shellcheck")
      (start-process-shell-command "nix-env" "nix-env" "nix-env -i ShellCheck"))
    (add-to-list 'flycheck-checkers 'sh-shellcheck t)
    (flycheck-select-checker 'sh-shellcheck)
    (flycheck-mode 1))
  :hook
  (sh-mode . akirak/setup-flycheck-shellcheck))

(provide 'setup-shell-scripts)
