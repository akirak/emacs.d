(defun akirak/current-org-agenda-key ()
  (pcase org-agenda-redo-command
    (`(org-agenda-run-series ,desc . ,_)
     (caar (cl-remove-if-not (lambda (list) (equal (nth 1 list) desc))
                             org-agenda-custom-commands)))))

(defmacro akirak/in-custom-agenda (key)
  "A helper for defining a custom agenda context.

Example:

  (setq org-capture-templates-contexts
        `((\".\" \"wt\" (,(akirak/in-custom-agenda \"w\")))))

"
  `(lambda () (equal (akirak/current-org-agenda-key) ,key)))

(provide 'init-org-contexts)
