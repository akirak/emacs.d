(defhydra org-insert-hydra (:hint nil)
  "
Insert into org-mode

^^From URL
^^--------
_l_ link
"
  ("l" org-web-tools-insert-link-for-url :exit t))

(provide 'org-insert-hydra)
;;; org-insert-hydra.el ends here
