(require 'hydra)

(defun akirak/org-hydra--set-property (name &optional value)
  (let ((initial (org-entry-get nil name)))
    (org-set-property name value)
    (let ((new-value (org-entry-get nil name)))
      (unless (equal initial new-value)
        (message "%s set to %s" name (prin1-to-string new-value))))))

(defun akirak/org-hydra--modify-property (name func)
  (let ((initial (org-entry-get nil name)))
    (condition-case nil
        (let ((new-value (funcall func initial)))
          (org-set-property name new-value)
          (message "%s set to %s" name (prin1-to-string new-value)))
      (error nil))))

(defhydra akirak/org-hydra
  (:hint nil)
  ;; TODO: Display the states of properties
  "
^^Insert           ^^Set           ^^Refile
^^---------------  ^^------------  ^^------
_il_ link for url  _si_ CUSTOM ID  _ra_ avy
_iw_ web page      _sh_ habit
^^                 _so_ ordered
"
  ("si" (akirak/org-hydra--set-property "CUSTOM_ID"))
  ("sh" (if (equal (org-entry-get nil "STYLE") "habit")
            (org-delete-property "STYLE")
          (akirak/org-hydra--set-property "STYLE" "habit")))
  ("so" (if (org-entry-get nil "ORDERED")
            (org-delete-property "ORDERED")
          (akirak/org-hydra--set-property "ORDERED" "t")))
  ("ra" avy-org-refile-as-child)
  ("il" org-web-tools-insert-link-for-url :exit t)
  ("iw" org-web-tools-insert-web-page-as-entry :exit t)
  ("q" nil "quit"))

(defalias 'akirak/org-hydra 'akirak/org-hydra/body)

(provide 'org-hydra)
