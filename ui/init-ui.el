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

(use-package highlight-indent-guides
  :init
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode))

(use-package fill-column-indicator
  :init
  (add-hook 'prog-mode-hook 'fci-mode))

(use-package whitespace
  :straight nil
  :diminish whitespace-mode
  :hook
  (prog-mode-hook . whitespace-mode)
  :custom
  (whitespace-display-mappings
   ;; all numbers are Unicode codepoint in decimal. try (insert-char 182 ) to see it
   '(
     (space-mark 32 [183] [46]) ; 32 SPACE, 183 MIDDLE DOT 「·」, 46 FULL STOP 「.」
     (newline-mark 10 [182 10]) ; 10 LINE FEED
     (tab-mark 9 [187 9] [9655 9] [92 9]) ; 9 TAB, 9655 WHITE RIGHT-POINTING TRIANGLE 「▷」
     ))
  (whitespace-style '(face tabs trailing tab-mark)))

(use-package git-gutter
  :diminish git-gutter-mode
  :init
  (global-git-gutter-mode))

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
