(use-package avy
  :init
  (require 'avy-extra-commands))

(use-package iedit)

(use-package comment-dwim-2)

(autoload 'narrow-or-widen-dwim "narrow-or-widen.el")

(provide 'init-prog-commands)
