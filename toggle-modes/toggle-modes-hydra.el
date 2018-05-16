(require 'hydra)

(defhydra akirak/toggle-modes-hydra
  (:hint nil)
  "
_c_ flycheck  [%s(if flycheck-mode \"on\" \"off\")]
_d_ eldoc     [%s(if eldoc-mode \"on\" \"off\")]
_s_ spell     [%s(if flyspell-mode \"on\" \"off\")]
"
  ("c" flycheck-mode)
  ("d" eldoc-mode)
  ("s" flyspell-mode))

(provide 'toggle-modes-hydra)
