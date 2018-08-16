;;; init-corefighter.el --- corefighter -*- lexical-binding: t -*-

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
        '((corefighter-git-statuses :title "Dirty Git repositories")
          (corefighter-git-statuses :title "Git repositories with stashes"
                                    :fields (stash))
          (corefighter-git-statuses :title "Repositories with unmerged branches"
                                    :fields (unmerged))
          (corefighter-org-ql :title "Org scheduled"
                              :due earlier
                              :sort (scheduled)
                              :q (and (or (scheduled <= today)
                                          (deadline <= today))
                                      (not (todo "DONE" "ARCHIVED"))
                                      (not (tags "ARCHIVE"))))))
  (corefighter-load-modules))

(use-package helm-corefighter
  :straight (helm-corefighter :host github :repo "akirak/corefighter.el"
                              :files ("helm-corefighter.el"))
  :commands (helm-corefighter))

(akirak/define-frame-workflow "inbox"
  :key "i"
  :layout '(progn
             (org-agenda nil "a")
             (delete-other-windows)
             (corefighter-sidebar)))

(provide 'init-corefighter)
;;; init-corefighter.el ends here
