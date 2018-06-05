;;;; M-g

(general-def "M-g c" #'avy-goto-char-in-line)

(general-def "M-g f" 'akirak/link-hint-open-link)

(akirak/define-contextual-key "M-g u"
  ('outline-up-heading :package 'outline :keymaps 'outline-minor-mode-map)
  ('org-up-element :package 'org :keymaps 'org-mode-map))

(general-def "M-g w" #'akirak/avy-goto-symbol-in-line)

;;;;; A bunch of avy commands to a symbol in line

(general-def :prefix "M-g"
  "p" #'akirak/avy-goto-defun-above
  "n" #'akirak/avy-goto-defun-below
  "-" #'akirak/avy-goto-word-in-line
  "'" #'akirak/avy-goto-quote-in-line
  "\"" #'akirak/avy-goto-dquote-in-line
  "[" #'akirak/avy-goto-open-bracket-above-in-defun
  "]" #'akirak/avy-goto-open-bracket-below-in-defun)

;;;; M-s

(akirak/define-contextual-key "M-s i"
  ('counsel-imenu)
  ('counsel-org-goto :package 'org :keymaps 'org-mode-map))


(akirak/define-contextual-key "M-s o"
  ('counsel-outline)
  ('counsel-org-goto :package 'org :keymaps 'org-mode-map))

;;;; Other keybindings under meta key
(general-def
  "M-r" 'ivy-resume
  "M-;" 'comment-dwim-2
  "M-/" 'hippie-expand)

(general-def :package 'company :keymaps 'company-mode-map
  "C-M-/" '(company-complete-common))

(general-def :keymaps 'outline-minor-mode-map :package 'outshine
  "M-RET" 'outshine-insert-heading)
(general-unbind :keymaps 'lispy-mode-map :package 'lispy "M-RET")

;; M-TAB is C-M-i
(general-def "M-TAB" 'complete-symbol)
(general-unbind :keymaps 'outline-minior-mode-map :package 'outshine "M-TAB")
(general-unbind :keymaps 'flyspell-mode-map :package 'flyspell "M-TAB")

(provide 'init-meta-keys)
