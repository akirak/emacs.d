(use-package yankpad
  :after (org yasnippet)
  :custom
  (yankpad-file (expand-file-name "yankpad/yankpad.org" no-littering-etc-directory)))

(defhydra yankpad-hydra (:hint nil)
  "
Yankpad: Category: %s`yankpad-category (_C_: Set, _A_: Append)
         _i_nsert, _r_epeat, _c_apture, _a_ya-persist, _E_dit, _R_eload
"
  ("C" yankpad-set-category)
  ("A" yankpad-append-category)
  ("i" yankpad-insert :exit t)
  ("r" yankpad-repeat :exit t)
  ("a" yankpad-aya-persist)
  ("c" yankpad-capture-snippet :exit t)
  ("E" yankpad-edit :exit t)
  ("R" yankpad-reload :exit t))

(defun akirak/yankpad-insert (&optional arg)
  (interactive "P")
  (if arg
      (yankpad-hydra/body)
    (yankpad-insert)))

(provide 'setup-yankpad)
