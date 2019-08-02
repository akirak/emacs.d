(use-package flyspell
  :when (executable-find "ispell")
  :config
  (defun akirak/turn-on-flyspell-text-mode ()
    (flyspell-mode 1))
  (defun akirak/turn-on-flyspell-prog-mode-conditionally ()
    (flyspell-prog-mode))
  :hook
  ((text-mode . akirak/turn-on-flyspell-text-mode)
   (prog-mode . akirak/turn-on-flyspell-prog-mode-conditionally)))

(general-unbind :keymaps 'flyspell-mode-map :package 'flyspell
  "C-," "C-." "C-M-i" "C-c $" "C-;")

(provide 'setup-spell)
