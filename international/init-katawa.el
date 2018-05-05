(use-package katawa
  :straight (katawa :host github :repo "akirak/katawa.el")
  :functions (katawa-get-some-candidates katawa-google--request))

(use-package katawa-ivy
  :straight katawa
  :after katawa
  :commands (katawa-ivy katawa-ivy-fix katawa-ivy-fix-at-point)
  :init
  (bind-keys :map text-mode-map
             ("C-c j" . katawa-ivy)
             ("C-c J" . katawa-ivy-fix)))

(provide 'init-katawa)
