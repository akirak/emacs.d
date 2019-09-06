(use-package yankpad
  :config
  (akirak/bind-register
    "M-y" #'yankpad-repeat))

(defhydra yankpad-hydra (:hint nil)
  "
Yankpad: Category: %s`yankpad-category (_C_: Set, _A_: Append)
         _i_nsert, _r_epeat, _c_apture, _a_ya-persist, _E_dit, _R_eload
"
  ("C" yankpad-set-category)
  ("A" yankpad-append-category)
  ("i" yankpad-insert :exit t)
  ("a" yankpad-aya-persist)
  ("c" yankpad-capture-snippet :exit t))

(provide 'setup-yankpad)
