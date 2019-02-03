;; Somehow Emacs seems to receive an infinite sequence of right keys
;; after receiving focus on Chrome OS, even without my config.
;; Is this a bug with Emacs or a hardware problem?
;; This workaround seems to prevent the issue. I don't use arrow keys
;; to move the cursor, so this is not a serious problem.
;; I've gained peace of mind by disabling one of the arrow keys
;; for cursor motion.
(general-unbind "<right>")

;; It seems that the pop-up menus hang up Chrome OS when Emacs is run
;; on Crostini. I don't know why, but it is safer to entirely disable
;; the pop-up commands.
(dolist (cmd '(mouse-buffer-menu
               mouse-popup-menubar
               mouse-appearance-menu
               mouse-major-mode-menu
               mouse-minor-mode-menu))
  (advice-add cmd :override 'keyboard-quit))

(general-unbind
  [C-down-mouse-1]
  [C-down-mouse-2]
  [C-down-mouse-3])

(provide 'setup-chromeos)
