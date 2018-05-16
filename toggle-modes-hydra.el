(require 'hydra)

(defhydra akirak/toggle-modes-hydra
  (:hint nil)
  "
Toggle modes

_c_ flycheck  [%s(if flycheck-mode (or flycheck-checker \"on\") \"off\")]
_d_ eldoc     [%s(if eldoc-mode \"on\" \"off\")]
_s_ spell     [%s(if flyspell-mode \"on\" \"off\")]

"
  ("c" flycheck-mode)
  ("d" eldoc-mode)
  ("s" flyspell-mode)

  (",c" flycheck-select-checker "Select flycheck-checker")

  ("q" nil "quit"))

(provide 'toggle-modes-hydra)
