(require 'init-app-map)
(require 'init-ui-map)

(general-def
  "<M-f1>" 'woman
  "<S-f2>" 'org-starter-find-file-by-key
  "<f5>" 'revert-buffer
  "<f6>" 'aweshell-new
  "<S-f6>" 'aweshell-next
  "<f7>" #'akirak/magit-status-prefer-existing
  "<f8>" 'akirak/ui-map
  "<f9>" 'recompile
  "<S-f9>" 'helm-make
  "<M-f10>" 'menu-bar-mode
  "<f12>" 'akirak/app-map
  "<S-f12>" #'corefighter-next-item)

;; Most of these commands are from crux and fwb-cmds.el
(general-def :prefix "<f2>"
  "D" #'crux-delete-file-and-buffer
  "o" #'crux-open-with
  "a" #'helm-org-rifle-agenda-files
  "c" #'akirak/pop-up-org-clocking-task
  "g" #'helm-repom
  "l" #'counsel-locate
  "r" #'counsel-recentf
  "t" #'aweshell-dedicated-open
  "R" #'crux-rename-file-and-buffer
  "S" #'sudo-find-file)

;; (with-eval-after-load 'origami
;;   (smartrep-define-key
;;       origami-mode-map "<f8>"
;;     '(("o" . origami-show-only-node)
;;       ("," . origami-undo)
;;       ("." . origami-redo)
;;       ("TAB" . origami-recursively-toggle-node)
;;       ("<backtab>" . origami-close-all-nodes))))

(provide 'init-func-keys)
