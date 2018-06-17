(general-def :prefix-map 'akirak/fix-map
  :prefix-command 'akirak/fix-map
  "e" 'akirak/hydra-flycheck
  "u" 'fix-word-upcase
  "l" 'fix-word-downcase
  "c" 'fix-word-capitalize
  ;; M-SPC was originally bound to just-one-space
  "SPC" 'just-one-space)

(provide 'init-fix-map)
