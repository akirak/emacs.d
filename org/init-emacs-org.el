(org-starter-define-file "emacs.org"
  :directory user-emacs-directory
  :key "e"
  :agenda t
  :refile '(:maxlevel . 3)
  :capture '(("e" "Emacs (emacs.org)")
             ("ei" "Emacs issue" entry (file+olp "Issues")
              "* TODO %?\n\n")
             ("et" "Emacs tip" entry (file+olp "Usage tips")
              "* %?\n\n")
             ("ep" "Emacs policy/overview" entry (file+olp "Policies")
              "* %?\n\n")))

(provide 'init-emacs-org)
