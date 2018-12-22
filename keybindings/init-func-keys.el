(require 'init-app-map)
(require 'init-ui-map)

(general-def
  "<f5>" 'revert-buffer
  "<f6>" 'akirak/shell-new
  "<f7>" #'akirak/magit-status-prefer-existing
  "<f8>" 'akirak/ui-map
  "<f9>" 'recompile
  "<f12>" 'akirak/app-map)

;; (with-eval-after-load 'origami
;;   (smartrep-define-key
;;       origami-mode-map "<f8>"
;;     '(("o" . origami-show-only-node)
;;       ("," . origami-undo)
;;       ("." . origami-redo)
;;       ("TAB" . origami-recursively-toggle-node)
;;       ("<backtab>" . origami-close-all-nodes))))

(general-def :prefix "<f1>"
  "x c" #'describe-char
  "x f" #'counsel-describe-face)

(provide 'init-func-keys)
