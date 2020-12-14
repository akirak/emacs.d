;; Install mozc_emacs_helper as well as mozc.el using Nix
(add-to-list 'load-path (expand-file-name "contrib/mozc" user-emacs-directory))

(straight-use-package `(mozc :type built-in))

(use-package mozc)

(use-package mozc-cand-posframe
  :after mozc
  :if (posframe-workable-p)
  :config
  (setq mozc-candidate-style 'posframe))

(use-package mozc-temp
  :disabled t
  :commands (mozc-temp-convert mozc-temp-convert-dwim))

(provide 'setup-japanese-mozc)
