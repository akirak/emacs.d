;;; setup-corefighter.el --- corefighter -*- lexical-binding: t -*-

(straight-use-package '(corefighter-extras :host github
                                           :repo "akirak/corefighter-extras"))

(require 'init-repom)
(require 'init-org-ql)

(use-package corefighter
  :after (repom)
  :defer 5
  :straight (corefighter :host github
                         :repo "akirak/corefighter.el"
                         :files
                         (:defaults
                          (:exclude "helm-corefighter.el")))
  :commands (corefighter-sidebar)
  :config
  (use-package corefighter-git-statuses
    :straight corefighter-extras)
  (use-package corefighter-org-ql
    :straight corefighter-extras)
  (setq corefighter-modules
        '((corefighter-git-statuses)
          (corefighter-git-statuses :fields (stash))
          ;; (corefighter-git-statuses :fields (unmerged))
          (corefighter-org-ql :title "Org scheduled"
                              :due earlier
                              :sort (scheduled)
                              :q (and (or (scheduled <= today)
                                          (deadline <= today))
                                      (not (todo "DONE" "ARCHIVED"))
                                      (not (tags "ARCHIVE"))))
          (corefighter-org-ql :title "Org next"
                              :q (and (todo "NEXT")
                                      (not (scheduled))))))
  (corefighter-load-modules))

(use-package helm-corefighter
  :straight (helm-corefighter :host github :repo "akirak/corefighter.el"
                              :files ("helm-corefighter.el"))
  :commands (helm-corefighter))

(provide 'setup-corefighter)
;;; setup-corefighter.el ends here
