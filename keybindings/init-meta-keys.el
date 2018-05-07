(general-def
  "M-g u" '(outline-up-heading :package outline :keymaps outline-minor-mode-map)
  "M-g u" '(org-up-element :package org :keymaps org-mode-map)
  "M-g f" 'akirak/link-hint-open-link
  "M-s i" 'counsel-imenu
  "M-s i" '(counsel-org-goto :package org :keymaps org-mode-map)
  "M-s o" 'counsel-outline
  ;; "M-o" '(nil :package lispy :keymaps lispy-mode-map)
  "M-r" 'ivy-resume
  "M-R" 'helm-resume
  "M-z" 'zop-to-char
  ;; M-TAB is C-M-i
  "M-RET" '(outshine-insert-heading :keymaps outline-minor-mode-map :package outshine)
  "M-RET" '(:keymaps lispy-mode-map :package lispy)
  "M-;" 'comment-dwim-2
  "M-/" 'hippie-expand
  "C-M-/" '(company-complete-common :package company :keymaps company-mode-map))

(general-def "M-o" 'ace-window)
(general-unbind :package 'lispy :keymaps 'lispy-mode-map "M-o")

(general-def "M-TAB" 'complete-symbol)
(general-unbind :keymaps 'outline-minior-mode-map :package 'outshine "M-TAB")
(general-unbind :keymaps 'flyspell-mode-map :package 'flyspell "M-TAB")

(provide 'init-meta-keys)
