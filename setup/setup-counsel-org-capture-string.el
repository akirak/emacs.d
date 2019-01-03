;;; setup-counsel-org-capture-string.el --- Configuration for counsel-org-capture-string -*- lexical-binding: t -*-

(use-package counsel-org-capture-string
  :straight (counsel-org-capture-string :host github
                                        :repo "akirak/counsel-org-capture-string")
  :commands (counsel-org-capture-string)
  :config
  (ivy-add-actions
   'counsel-org-capture-string
   '(("sd" akirak/web-search-firefox "Default search with Firefox")
     ("sg" akirak/surfraw/google "Google")
     ("sl" akiraksearch/lucky "I'm Feeling Lucky")
     ("ss" akirak/helm-search "Choose a search engine")))
  (defun akirak/counsel-org-capture ()
    (require 'org-capture)
    (require 'counsel-org-capture-string)
    (ivy-read "Capture template: "
              #'counsel-org-capture-string--template-list
              :action (lambda (x)
                        (org-capture nil (car (split-string x))))))
  (advice-add #'counsel-org-capture :override #'akirak/counsel-org-capture))

(provide 'setup-counsel-org-capture-string)
;;; setup-counsel-org-capture-string.el ends here
