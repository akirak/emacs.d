;; Use diminish to reduce clutters from the modeline
(use-package diminish
  :init
  (diminish 'auto-revert-mode)
  (diminish 'outline-minor-mode)
  (diminish 'flyspell-mode))

(provide 'init-diminish)
