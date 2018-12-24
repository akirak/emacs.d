;; Turn off global-eldoc-mode, as I use some alternative help systems
;; e.g. lsp-mode, depending on the language.
(global-eldoc-mode -1)

;;;; Project management
(require 'init-projectile)

;;;; Jump and navigation
(require 'init-dumb-jump)
(require 'init-avy)
(require 'init-link-hint)
(require 'init-bm)

;;;; Expansion, completion, and templates
(require 'init-lsp)
(require 'init-hippie-expand)
(require 'init-yasnippet)
(require 'init-yankpad)
(require 'init-autoinsert)

;;;; Other packages for quality of life
;; Small commands
(require 'init-prog-commands)

;;;;; Editing
(require 'init-aggressive-indent)
(require 'init-corral)
(require 'init-fix-word)
(require 'init-easy-kill)
(require 'init-expand-region)
(require 'init-flycheck)
(require 'init-embrace)

;;;;; Visual
(require 'init-symbol-overlay)
(require 'init-rainbow-mode)

;;;;; Referencing
(require 'init-dash)

;;;;; Misc
(require 'init-scratch)

;;;; Org
(require 'init-outshine)
(require 'setup-outorg)

(provide 'init-coding)
