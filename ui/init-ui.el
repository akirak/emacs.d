;;;; General appearance
;;;;; Frame elements
;; (require 'init-spaceline-ati)
;; (require 'init-doom-modeline)
(require 'init-feebleline)

;; The header line
(require 'init-header-line)
(require 'init-frame-title)

;;;;; Prefered theme
(require 'init-dracula-theme)

;;;;; Typography
;; Activate the primary typography if and only if a window system is
;; available
(when (window-system)
  (require 'init-typography))

;;;; Visual cues

;;;;; Highlighting focus
;; Use hl-line-mode
(add-hook 'prog-mode-hook (lambda () (hl-line-mode 1)))
(add-hook 'text-mode-hook (lambda () (hl-line-mode 1)))

(require 'init-focus)
(require 'init-beacon)
(require 'init-dimmer)
;; (require 'init-origami)

;;;;; Text decoration
(require 'init-rainbow-delimiters)
(require 'init-fontify-face)

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
;; (require 'init-corefighter)

(provide 'init-ui)
