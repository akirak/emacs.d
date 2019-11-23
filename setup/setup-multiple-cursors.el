(use-package multiple-cursors)

(general-def
  "<S-up>" #'mc/mark-previous-like-this
  "<S-down>" #'mc/mark-next-like-this)

(general-def
  "C-S-SPC" #'set-rectangular-region-anchor)

(akirak/bind-user
  "|" #'mc/edit-lines)

;; TODO: Refine this map
(akirak/bind-mark
  "a" #'mc/mark-all-dwim
  "b" #'mc/edit-beginnings-of-lines
  "e" #'mc/edit-ends-of-lines
  "n" #'mc/mark-next-like-this
  "p" #'mc/mark-previous-like-this
  "s" #'mc/skip-to-next-like-this
  "u" #'mc/unmark-next-like-this
  "w" #'mc/mark-next-word-like-this
  "'" #'mc/mark-next-symbol-like-this
  "SPC" #'mc/mark-pop)

(provide 'setup-multiple-cursors)
