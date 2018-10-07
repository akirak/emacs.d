;;;; General appearance
(require 'init-font)

;;;;; Modeline
;; (require 'init-spaceline-ati)
;; (require 'init-doom-modeline)
(require 'init-feebleline)

;; The header line
(require 'init-header-line)
(require 'init-frame-title)

;;;;; Prefered themes
(require 'init-dracula-theme)

;;;; Visual cues

;;;;; Focus
;; Use hl-line-mode
(add-hook 'prog-mode-hook (lambda () (hl-line-mode 1)))
(add-hook 'text-mode-hook (lambda () (hl-line-mode 1)))

(require 'init-focus)
(require 'init-beacon)
(require 'init-dimmer)
;; (require 'init-origami)

;;;;; Text
(require 'init-rainbow-delimiters)

;;;;; Columns and spaces
(require 'init-fci)
(require 'init-highlight-indent-guides)
(require 'init-whitespace)

;;;;; Fringe
(require 'init-line-numbers)
(require 'init-git-gutter)

;;;; Window layout
(require 'init-shackle)

;;;; Switching windows/frames
(require 'init-frames)
;; (require 'init-switch-window)
(require 'init-ace-window)

;;;; Distraction-free editing
(require 'init-olivetti)

;;;; Extra information displays
(require 'init-imenu-list)
(require 'init-sidebar)
(require 'init-corefighter)

(provide 'init-ui)
