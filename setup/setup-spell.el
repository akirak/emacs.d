(use-package flyspell
  :when (executable-find "ispell")
  :hook
  ((text-mode . (lambda () (flyspell-mode 1)))
   ((change-log-mode log-edit-mode) . (lambda () (flyspell-mode -1)))
   (prog-mode . flyspell-prog-mode)))

(general-unbind :keymaps 'flyspell-mode-map :package 'flyspell
  "C-," "C-." "C-M-i" "C-c $" "C-;")

(provide 'setup-spell)
