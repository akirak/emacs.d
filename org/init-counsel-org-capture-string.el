;;; init-counsel-org-capture-string.el --- Configuration for counsel-org-capture-string -*- lexical-binding: t -*-

(use-package counsel-org-capture-string
  :straight (counsel-org-capture-string :host github
                                        :repo "akirak/counsel-org-capture-string")
  :commands (counsel-org-capture-string)
  :config
  (ivy-add-actions
   'counsel-org-capture-string
   '(("sd" akirak/web-search-firefox "Default search with Firefox")
     ("sg" akirak/surfraw/google "Google"))))

(provide 'init-counsel-org-capture-string)
;;; init-counsel-org-capture-string.el ends here
