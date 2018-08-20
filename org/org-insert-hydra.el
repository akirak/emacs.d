(defhydra org-insert-hydra (:hint nil :exit t)
  "
Insert into org-mode

^^From URL
^^--------
_l_ link

"
  ("l" org-web-tools-insert-link-for-url)
  ("q" (akirak/org-insert-block "QUOTE") "quote")
  ("s" (progn
         (akirak/org-insert-block "SRC")
         (end-of-line 0)
         (insert " ")) "source")
  ("t" (if (org-at-table-p)
           (org-table-hydra/body)
         (org-table-create)) "table"))

(defun akirak/org-insert-block (type &optional args)
  (insert "#+BEGIN_" type)
  (when args
    (insert args))
  (insert "\n\n#+END_" type "\n")
  (beginning-of-line -1))

(defhydra org-table-hydra (:hint nil)
  "
Org Table

        ^^Insert  ^^Delete
Row     _ir_      _dr_
Column  _ic_      _dc_

Edit: _e_
Navigation: _n_ _p_ _f_ _b_
"
  ("dc" org-table-delete-column)
  ("dr" org-table-kill-row)
  ("ic" org-table-insert-column)
  ("ir" org-table-insert-row)
  ("e" org-edit-special)
  ("f" org-table-next-field)
  ("b" org-table-previous-field)
  ("n" org-table-next-row)
  ("p" previous-line))

(provide 'org-insert-hydra)
;;; org-insert-hydra.el ends here
