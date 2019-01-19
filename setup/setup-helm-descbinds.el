(use-package helm-descbinds :after helm
  :commands (helm-descbinds)
  :bind
  ([help ?b] . helm-descbinds)
  :custom
  (helm-descbinds-window-style 'split "Use pop-up style window for descbinds"))

(provide 'setup-helm-descbinds)
