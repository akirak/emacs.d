(require 'hydra)

(defhydra akirak/org-hydra
  (:hint nil)
  "
^^Insert           ^^Set           ^^Refile
^^---------------  ^^------------  ^^------
_il_ link for url  _si_ CUSTOM ID  _ra_ avy
_iw_ web page      _sh_ habit

"
  ("si" (org-set-property "CUSTOM_ID" nil))
  ("sh" (org-set-property "STYLE" "habit"))
  ("ra" avy-org-refile-as-child)
  ("il" org-web-tools-insert-link-for-url :exit t)
  ("iw" org-web-tools-insert-web-page-as-entry :exit t)
  ("q" nil "quit"))

(defalias 'akirak/org-hydra 'akirak/org-hydra/body)

(provide 'org-hydra)
