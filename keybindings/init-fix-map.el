(akirak/bind-fix-map
 :keymaps '(prog-mode-map text-mode-map)
 "e" 'akirak/hydra-flycheck
 "u" 'fix-word-upcase
 "l" 'fix-word-downcase
 "c" 'fix-word-capitalize
 ;; M-SPC was originally bound to just-one-space
 "SPC" 'just-one-space)

(provide 'init-fix-map)
