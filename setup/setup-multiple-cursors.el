(use-package multiple-cursors)

(general-def
  "C-<" #'mc/mark-previous-like-this
  "C->" #'mc/mark-next-like-this)

(akirak/bind-user
  "|" #'mc/edit-lines)

;; TODO: Refine this map
(akirak/bind-mark
  "a" #'mc/mark-all-dwim
  "b" #'mc/edit-beginnings-of-lines
  "e" #'mc/edit-ends-of-lines
  "s" #'mc/skip-to-next-like-this
  "u" #'mc/unmark-next-like-this
  "SPC" #'mc/mark-pop)

(provide 'setup-multiple-cursors)
