;;;; M-g

(general-def
  "M-g f" 'akirak/link-hint-open-link)

(akirak/define-contextual-key "M-g u"
  ('outline-up-heading :package 'outline :keymaps 'outline-minor-mode-map)
  ('org-up-element :package 'org :keymaps 'org-mode-map))

;;;; M-s

(akirak/define-contextual-key "M-s i"
  ('counsel-imenu)
  ('counsel-org-goto :package 'org :keymaps 'org-mode-map))

(general-def
    "M-s o" 'counsel-outline)

;;;; Other keybindings under meta key

(general-def
  ;; "M-o" '(nil :package lispy :keymaps lispy-mode-map)
  "M-r" 'ivy-resume
  "M-R" 'helm-resume
  "M-z" 'zop-to-char
  ;; M-TAB is C-M-i
  "M-;" 'comment-dwim-2
  "M-/" 'hippie-expand)

(general-def :package 'company :keymaps 'company-mode-map
  "C-M-/" '(company-complete-common))

(general-def :keymaps 'outline-minor-mode-map :package 'outshine
  "M-RET" 'outshine-insert-heading)
(general-unbind :keymaps 'lispy-mode-map :package 'lispy "M-RET")

(general-def "M-o" 'ace-window)
(general-unbind :package 'lispy :keymaps 'lispy-mode-map "M-o")

(general-def "M-TAB" 'complete-symbol)
(general-unbind :keymaps 'outline-minior-mode-map :package 'outshine "M-TAB")
(general-unbind :keymaps 'flyspell-mode-map :package 'flyspell "M-TAB")

(provide 'init-meta-keys)
