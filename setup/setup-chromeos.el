;; Somehow Emacs seems to receive an infinite sequence of right keys
;; after receiving focus on Chrome OS, even without my config.
;; Is this a bug with Emacs or a hardware problem?
;; This workaround seems to prevent the issue. I don't use arrow keys
;; to move the cursor, so this is not a serious problem.
;; I've gained peace of mind by disabling one of the arrow keys
;; for cursor motion.
(general-unbind "<right>")

(provide 'setup-chromeos)
