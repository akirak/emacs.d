;;;; M-g

(general-def "M-g c" #'avy-goto-char-in-line)

(general-def "M-g f" 'akirak/link-hint-open-link)

(akirak/define-contextual-key "M-g h"
  ('outline-up-heading :package 'outline :keymaps 'outline-minor-mode-map)
  ('org-up-element :package 'org :keymaps 'org-mode-map))

(general-def "M-g w" #'akirak/avy-goto-symbol-in-line)

(general-def :prefix "M-g"
  "," #'dumb-jump-back
  "." #'dumb-jump-go
  "j" #'dumb-jump-go-other-window
  ;; "i" #'dumb-jump-go-prompt
  ;; "x" #'dumb-jump-go-prefer-external
  ;; "z" #'dumb-jump-go-prefer-external-other-window
  )

;;;;; A bunch of avy commands to a symbol in line

;; This is questionable.
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
;; TODO: Bind a more complex key
(general-def "M-m" 'er/expand-region)
;; (general-def :keymaps 'emacs-lisp-mode-map "M-m" 'er/mark-symbol-with-prefix)
;; (general-unbind :keymaps 'lispy-mode-map :package 'lispy "M-m")

(general-def "M-o" 'aya-open-line)
(general-unbind :keymaps 'lispy-mode-map :package 'lispy "M-o")

;; Use M-r as a prefix for register and repeat commands
(general-def
  "M-r" (general-simulate-key "C-x r")
  ;; M-r h
  "C-x r h" #'helm-resume
  ;; M-r ;
  "C-x r ;" #'repeat-complex-command
  ;; M-r M-r
  "C-x r M-r" #'ivy-resume)

(general-def
  "M-;" 'comment-dwim-2
  ;; "M-/" 'hippie-expand
  )

(general-def :package 'company :keymaps 'company-mode-map
  "M-/" #'company-complete)

(general-def :keymaps 'outline-minor-mode-map :package 'outshine
  "M-RET" 'outshine-insert-heading)
(general-unbind :keymaps 'lispy-mode-map :package 'lispy "M-RET")

;; M-TAB is C-M-i
(general-def "M-TAB" 'complete-symbol)
(general-unbind :keymaps 'flyspell-mode-map :package 'flyspell "M-TAB")

(general-create-definer akirak/bind-fix-map
  :prefix-map 'akirak/fix-map
  :prefix "M-SPC")

(akirak/bind-fix-map "M-SPC" #'ace-window)

(require 'init-fix-map)

(general-def "C-M-g" #'frame-workflow-prefix-map)

(global-unset-key (kbd "C-M-o"))
(general-create-definer akirak/bind-global-org-map
  :prefix "C-M-o")

(require 'init-global-org-map)

(provide 'init-meta-keys)
