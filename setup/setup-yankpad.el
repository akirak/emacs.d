(use-package yankpad
  :after (org yasnippet))

(defhydra yankpad-hydra (:hint nil)
  "
Category: %s`yankpad-category (_C_: Set, _A_: Append)
"
  ("C" yankpad-set-category)
  ("A" yankpad-append-category)
  ("SPC" yankpad-insert "Insert" :exit t)
  ("r" yankpad-repeat "Repeat" :exit t)
  ("a" yankpad-aya-persist "Aya-persist")
  ("c" yankpad-capture-snippet "Capture" :exit t)
  ("E" yankpad-edit "Edit" :exit t)
  ("R" yankpad-reload "Reload" :exit t))

(defun akirak/yankpad-insert (&optional arg)
  (interactive "P")
  (if arg
      (yankpad-hydra/body)
    (yankpad-insert)))

(provide 'setup-yankpad)
