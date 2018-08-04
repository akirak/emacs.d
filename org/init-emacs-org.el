(org-starter-define-file "emacs.org"
  :directory user-emacs-directory
  :key "e"
  :agenda t
  :refile '(:maxlevel . 3)
  :capture `(("e" "Emacs" entry (file+function
                                 (lambda ()
                                   (goto-char (org-find-property "CUSTOM_ID" "issues"))))
              ,(akirak/org-capture-entry-template-1 "%i%?" ""
                                                    :todo "TODO"))))

(provide 'init-emacs-org)
