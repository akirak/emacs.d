(akirak/bind-fix-map
  :keymaps '(prog-mode-map text-mode-map)
  "e" 'akirak/hydra-flycheck
  "u" 'fix-word-upcase
  "l" 'fix-word-downcase
  "c" 'fix-word-capitalize
  "s" #'akirak/kill-sentence
  "w" #'akirak/kill-word
  "f" #'akirak/kill-defun
  "o" #'split-line     ; Originally C-M-o
  ;; M-SPC was originally bound to just-one-space
  "SPC" 'just-one-space)

(provide 'init-fix-map)
