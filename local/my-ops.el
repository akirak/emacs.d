(org-starter-def "~/ops"
  :id ops
  :agenda t
  :refile (:maxlevel . 3)
  :add-to-path nil
  :config
  (add-to-list 'load-path "~/ops/site-lisp/org/")
  (require 'my-org-config)
  (org-starter-define-file "yankpad.org"
    :directory "~/ops/templates/"
    :set-default 'yankpad-file
    :refile '(:level . 1))
  (provide 'my-ops))
