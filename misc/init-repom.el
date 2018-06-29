;;; init-repom.el --- Configuration for the repository manager -*- lexical-binding: t -*-

(use-package helm-projectile)

(use-package repom
  :straight (repom :host github :repo "akirak/repom.el")
  :commands (helm-repom))

(provide 'init-repom)
;;; init-repom.el ends here
