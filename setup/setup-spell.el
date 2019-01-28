(use-package flyspell
  :hook
  ((text-mode . (lambda () (flyspell-mode 1)))
   ((change-log-mode log-edit-mode) . (lambda () (flyspell-mode -1)))
   (prog-mode . flyspell-prog-mode)))

(general-unbind :keymaps 'flyspell-mode-map
  "C-," "C-." "C-M-i" "C-c $" "C-;")

(provide 'setup-spell)
