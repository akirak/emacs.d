(defhydra org-insert-hydra (:hint nil)
  "
Insert into org-mode

^^From URL  ^^Misc
^^--------  ^^--------
_l_ link    _t_ table
"
  ("l" org-web-tools-insert-link-for-url :exit t)
  ("t" (if (org-at-table-p)
           (org-table-hydra/body)
         (org-table-create)) :exit t))

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
