;; I won't use matrix-client.
;; matrix-client contains scratch.el, which conflicts with a package
;; of the same name.

(use-package matrix-client
  :straight (matrix-client :host github :repo "jgkamat/matrix-client-el"))

(provide 'init-matrix)
