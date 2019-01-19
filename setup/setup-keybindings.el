;; TODO: Add a global minor mode for these keybindings like [[https://github.com/kaushalmodi/.emacs.d][Kaushal Modi]] does?
(akirak/bind-key :prefix "<f1>"
  "M" #'woman)

(akirak/bind-key :prefix "<f1> x"
  "c" #'describe-char
  "f" #'counsel-describe-face)

;; (general-def
;;   "<M-prior>" #'git-gutter:previous-hunk
;;   "<M-next>" #'git-gutter:next-hunk
;;   "<M-insert>" #'git-gutter:stage-hunk)

(general-def "M-g f" 'akirak/link-hint-open-link)

(akirak/define-contextual-key "M-g h"
  ('outline-up-heading :package 'outline :keymaps 'outline-minor-mode-map)
  ('org-up-element :package 'org :keymaps 'org-mode-map))

(general-def "M-g w" #'akirak/avy-goto-symbol-in-line)

(general-def :prefix "M-g"
  "," #'dumb-jump-back
  "." #'dumb-jump-go)

;;;;; A bunch of avy commands to a symbol in line

(akirak/define-contextual-key "M-s i"
  ('counsel-imenu)
  ('counsel-org-goto :package 'org :keymaps 'org-mode-map))

(akirak/define-contextual-key "M-s o"
  ('counsel-outline)
  ('counsel-org-goto :package 'org :keymaps 'org-mode-map))

(akirak/bind-key "M-s M-o" #'helm-org-rifle-known-files)

;;;; Other keybindings under meta key
;; TODO: Bind a more complex key
;; (general-def "M-m" 'er/expand-region)
;; (general-def :keymaps 'emacs-lisp-mode-map "M-m" 'er/mark-symbol-with-prefix)
;; (general-unbind :keymaps 'lispy-mode-map :package 'lispy "M-m")


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
  )

(general-def :package 'company :keymaps 'company-mode-map
  "M-/" #'company-complete)

(general-def :keymaps 'outline-minor-mode-map :package 'outshine
  "M-RET" 'outshine-insert-heading)
(general-unbind :keymaps 'lispy-mode-map :package 'lispy "M-RET")

;; M-TAB is C-M-i
(general-def "M-TAB" 'complete-symbol)
(general-unbind :keymaps 'flyspell-mode-map :package 'flyspell "M-TAB")

(general-def "C-M-g" #'frame-workflow-prefix-map)

(global-unset-key (kbd "C-M-o"))

(general-def
  "<M-f12>" '((lambda ()
                (interactive)
                (shell-command "setxkbmap -option ctrl:nocaps"))
              :wk "setxkbmap"))

(general-def
  "C-c b" 'helm-bm
  "C-c c" #'org-capture
  "C-c i" 'scratch
  "C-c k" 'kill-compilation
  "C-c l" 'org-store-link
  "C-c m" 'bm-toggle
  "C-c n" #'counsel-org-capture-string
  "C-c p" 'yankpad-insert
  "C-c s" 'symbol-overlay-put
  "C-c u" #'winner-undo-repeat
  "C-c y" 'ivy-yasnippet
  "C-c '" #'outorg-edit-as-org

  "C-x /" #'counsel-rg
  "C-x =" #'narrow-or-widen-dwim

  "C-x D" #'crux-delete-file-and-buffer
  "C-x F" #'counsel-recentf
  "C-x L" #'counsel-locate
  "C-x R" #'crux-rename-file-and-buffer
  "C-x S" #'sudo-find-file
  "C-x T" #'akirak/shell-toggle-dedicated
  "C-x k" #'kill-this-buffer
  "C-x p" #'counsel-projectile
  "C-x t" #'helm-tail
  "C-x x" #'crux-open-with

  "C-x C-b" #'ibuffer
  "C-x C-j" #'dired-jump

  "C-;" 'iedit-mode)

(general-def :keymaps 'org-mode-map :package 'org
  "C-c o" nil)

;; (general-def origami-mode-map
;;   ;; "C-c o" #'origami-recursively-toggle-node
;;   "C-c o" #'origami-show-node)

(general-def :prefix "C-."
  "a" #'embrace-add
  "c" 'fix-word-capitalize
  "d" #'embrace-delete
  "j" #'katawa-ivy-fix-at-point
  "l" 'fix-word-downcase
  "o" #'split-line                      ; Originally C-M-o
  "p" #'embrace-change
  "u" 'fix-word-upcase
  ;; TODO: Maybe activate these keybindings
  ;; "s" #'akirak/kill-sentence
  ;; "w" #'akirak/kill-word
  ;; "f" #'akirak/kill-defun

  "SPC" 'just-one-space)

(akirak/define-contextual-key "C-'"
  ('avy-goto-char-timer)
  (nil :keymaps 'org-mode-map :package 'org))

;;;; Ctrl keys

;;;; Function keys
(general-def
  "<f5>" 'revert-buffer
  "<f6>" 'akirak/shell-new
  "<f7>" #'akirak/magit-status-prefer-existing
  "<f9>" 'recompile)

(general-def :prefix "<f12>"
  "c" 'calendar
  "d" 'helm-linux-disks
  "i" 'docker-images
  "k" 'docker-containers
  "p" 'prodigy
  "P" 'helm-system-packages
  "S" 'helm-systemd
  "t" 'helm-top
  "u" 'uptimes)

;;;; Misc

(general-def :prefix "M-s"
  "d" #'helm-dash)

;; "a" #'helm-dash-activate-docset
;; "d" #'helm-dash-at-point
;; "+" #'helm-dash-install-docset

(akirak/bind-global-org-map
  "a" #'org-agenda
  "b" #'ivy-switch-to-org-buffer
  ;; Maybe
  ;; "v" #'org-web-tools-read-url-as-org
  )

(general-def :keymaps 'org-mode-map :package 'org
  ;; I don't use any of these bindings and want to use them for other purposes
  "C-c [" nil
  "C-c ]" nil
  "M-n" 'org-metadown
  "M-p" 'org-metaup
  "M-H" 'org-shiftmetaleft
  "M-L" 'org-shiftmetaright
  "C-1" 'counsel-org-tag
  ;; TODO: Create hydra commands
  ;; "C-2" 'org-time-hydra
  ;; "C-3" 'org-edna-hydra
  "C-4" 'org-starter-refile-by-key
  "C-8" 'org-insert-hydra/body
  "C-9" #'org-tree-to-indirect-buffer)

;; Somehow Emacs seems to receive an infinite sequence of right keys
;; after receiving focus on Chrome OS, even without my config.
;; Is this a bug with Emacs or a hardware problem?
;; This workaround seems to prevent the issue. I don't use arrow keys
;; to move the cursor, so this is not a serious problem.
;; I've gained peace of mind by disabling one of the arrow keys
;; for cursor motion.
(general-unbind "<right>")

(provide 'setup-keybindings)
