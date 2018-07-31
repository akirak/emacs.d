;;; init-corefighter.el --- corefighter -*- lexical-binding: t -*-

(straight-use-package '(corefighter-extras :host github
                                           :repo "akirak/corefighter-extras"))

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
  (require 'corefighter-repom)
  (setq corefighter-modules
        '((corefighter-repom-dirty)
          (corefighter-org-agenda)))
  (corefighter-load-modules))

(use-package helm-corefighter
  :straight (helm-corefighter :host github :repo "akirak/corefighter.el"
                              :files ("helm-corefighter.el"))
  :commands (helm-corefighter))

(require 'init-repom)

(use-package corefighter-repom
  :straight corefighter-extras)

(akirak/define-frame-workflow "inbox"
  :key "i"
  :layout '(progn
             (org-agenda nil "a")
             (delete-other-windows)
             (corefighter-sidebar)))

(provide 'init-corefighter)
;;; init-corefighter.el ends here
