;;; magithub
;; This configuration is deprecated. Forge seems to be a more promising
;; package.See [[file:init-forge.el]].

;; Magithub is still useful for forking repositories, but
;; I am not sure if the situation continues in the future.

(use-package magithub
  :after magit
  :config
  (magithub-feature-autoinject 'status-checks-header))

(provide 'init-magithub)
