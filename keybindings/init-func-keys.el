(require 'init-app-map)
(require 'init-ui-map)

(general-def
  "<M-f1>" 'woman
  "<S-f2>" 'org-starter-find-file-by-key
  "<f5>" 'revert-buffer
  "<f6>" 'multi-term
  "<S-f6>" 'multi-term-next
  "<f7>" 'magit
  "<f8>" 'akirak/ui-map
  "<f9>" 'recompile
  "<S-f9>" 'helm-make
  "<M-f10>" 'menu-bar-mode
  "<f12>" 'akirak/app-map)

;; Most of these commands are from crux and fwb-cmds.el
(general-def :prefix "<f2>"
  "D" #'crux-delete-file-and-buffer
  "o" #'crux-open-with
  "a" #'helm-org-rifle-agenda-files
  "c" #'akirak/pop-up-org-clocking-task
  "g" #'helm-repom
  "l" #'counsel-locate
  "r" #'counsel-recentf
  "t" #'multi-term-dedicated-toggle
  "R" #'crux-rename-file-and-buffer
  "S" #'sudo-find-file)

(smartrep-define-key
    global-map "<f8>"
  '(("=" . text-scale-increase)
    ("-" . text-scale-decrease)))

(provide 'init-func-keys)
