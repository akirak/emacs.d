(use-package flyspell
  :when (executable-find "ispell"))

(general-unbind :keymaps 'flyspell-mode-map :package 'flyspell
  "C-," "C-." "C-M-i" "C-c $" "C-;")

(provide 'setup-spell)
