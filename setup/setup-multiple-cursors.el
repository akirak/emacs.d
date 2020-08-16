(use-package multiple-cursors)

(general-def
  "<S-up>" #'mc/mark-previous-like-this
  "<S-down>" #'mc/mark-next-like-this)

(general-def
  "C-S-SPC" #'set-rectangular-region-anchor)

(akirak/bind-user
  "|" #'mc/edit-lines)

;; TODO: Refine this map
(akirak/bind-generic
  "ma" #'mc/mark-all-dwim
  "mb" #'mc/edit-beginnings-of-lines
  "me" #'mc/edit-ends-of-lines
  "mn" #'mc/mark-next-like-this
  "mp" #'mc/mark-previous-like-this
  "ms" #'mc/skip-to-next-like-this
  "mu" #'mc/unmark-next-like-this
  "mw" #'mc/mark-next-word-like-this
  "m'" #'mc/mark-next-symbol-like-this
  "m SPC" #'mc/mark-pop)

(provide 'setup-multiple-cursors)
