;;; init-corefighter.el --- corefighter -*- lexical-binding: t -*-

(straight-use-package '(corefighter-extras :host github
                                           :repo "akirak/corefighter-extras"))

(use-package corefighter
  :straight (corefighter :host github
                         :repo "akirak/corefighter.el")
  :commands (corefighter-sidebar)
  :config
  (require 'corefighter-repom)
  (setq corefighter-modules
        '((corefighter-repom-dirty)
          (corefighter-org-agenda)))
  (corefighter-load-modules))

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
