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
(require 'init-company)
(require 'init-hippie-expand)
(require 'init-yasnippet)
(require 'init-yankpad)
(require 'init-autoinsert)

;;;; Compilation
(require 'init-helm-make)

;;;; Other packages for quality of life
;; Small commands
(require 'init-prog-commands)

;;;;; Editing
(require 'init-aggressive-indent)
(require 'init-corral)
(require 'init-fix-word)
(require 'init-expand-region)
(require 'init-flycheck)

;;;;; Visual
;; (require 'init-undo-tree)
(require 'init-anzu)
(require 'init-symbol-overlay)
(require 'init-rainbow-mode)
(require 'init-hl-todo)

;;;;; Referencing
(require 'init-dash)

;;;;; Misc
(require 'init-scratch)

;;;; Org
(require 'init-outshine)
(require 'init-outorg)

(provide 'init-coding)
