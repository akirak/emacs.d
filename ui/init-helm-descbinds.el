(require 'init-helm)

(use-package helm-descbinds
  :after helm
  :commands (helm-descbinds)
  :bind
  ([remap describe-bindings] . helm-descbinds)
  :custom
  (helm-descbinds-window-style 'split "Use pop-up style window for descbinds"))

(provide 'init-helm-descbinds)
