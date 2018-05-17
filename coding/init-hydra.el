(use-package hydra)

(straight-use-package '(hydra-collection
                        :host github :repo "akirak/hydra-collection"))

(use-package hydra-launcher
  :straight hydra-collection
  :commands (akirak/hydra-launcher-for-major-mode))

(use-package toggle-modes-hydra
  :straight hydra-collection
  :commands (akirak/toggle-modes-hydra/body))

(provide 'init-hydra)
