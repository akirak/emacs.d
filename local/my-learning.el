(org-starter-def "~/learning"
  :add-to-path t
  :config
  (load-file "~/learning/init.el")
  (org-starter-def "~/learning/natural-languages"
    :id language-learning
    :add-to-path t
    :agenda nil
    :refile (:maxlevel . 1)
    :files
    ("english.org")
    ("chinese.org")
    ("japanese.org"))
  (provide 'my-learning))
