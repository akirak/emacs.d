(add-hook 'prog-mode-hook (lambda () (hl-line-mode 1)))
(add-hook 'text-mode-hook (lambda () (hl-line-mode 1)))

(require 'init-dracula-theme)
(require 'init-visual-cues)
(require 'init-shackle)
(require 'init-font)
(require 'init-olivetti)
(require 'init-switch-window)
;; (require 'init-ace-window)
;; Deprecate treemacs in favor of sidebar
;; (require 'init-treemacs)
(require 'init-sidebar)
(require 'init-framemove)
(require 'init-frames)
(require 'init-modeline)
(require 'init-focus)
(require 'init-beacon)

(require 'init-anzu)

(provide 'init-ui)
