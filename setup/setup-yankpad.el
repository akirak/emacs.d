(defhydra yankpad-hydra (:hint nil)
  ("C" yankpad-set-category)
  ("A" yankpad-append-category)
  ("i" yankpad-insert :exit t)
  ("a" yankpad-aya-persist)
  ("c" yankpad-capture-snippet :exit t))

(provide 'setup-yankpad)
