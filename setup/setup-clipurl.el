(use-package clipurl
  :straight (clipurl :host github :repo "akirak/clipurl.el"))

(use-package ivy-clipurl
  :straight clipurl
  :commands (ivy-clipurl)
  :config
  (ivy-add-actions 'ivy-clipurl
                   '(("c" akirak/git-clone "git clone")
                     ("r" org-web-tools-read-url-as-org "Read as Org")
                     ("u" akirak/straight-use-package-git-url "use-package"))))

(provide 'setup-clipurl)
