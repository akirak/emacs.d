(general-def
  "C-x =" #'narrow-or-widen-dwim)

;; Windows
(akirak/bind-user
  "u" #'winner-undo-repeat)
(general-def
  "C-x T" #'akirak/shell-toggle-dedicated)

;; buffer management
(general-def
  "C-x k" #'kill-this-buffer  
  "C-x C-b" #'ibuffer)

;; Development
(akirak/bind-user
  "i" 'scratch)

(general-def
  "C-x t" #'helm-tail)

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

;; Somehow Emacs seems to receive an infinite sequence of right keys
;; after receiving focus on Chrome OS, even without my config.
;; Is this a bug with Emacs or a hardware problem?
;; This workaround seems to prevent the issue. I don't use arrow keys
;; to move the cursor, so this is not a serious problem.
;; I've gained peace of mind by disabling one of the arrow keys
;; for cursor motion.
(general-unbind "<right>")

(provide 'setup-keybindings)
