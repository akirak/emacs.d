;; Turn off global-eldoc-mode, as I use some alternative help systems
;; e.g. lsp-mode, depending on the language.
(global-eldoc-mode -1)

;;;; Expansion, completion, and templates
(require 'init-lsp)

;;;;; Editing
(require 'init-corral)
(require 'init-easy-kill)
(require 'init-expand-region)
(require 'init-flycheck)

;;;;; Visual
(require 'init-symbol-overlay)

;;;;; Referencing
(require 'init-dash)

(provide 'init-coding)
