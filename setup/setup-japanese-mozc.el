;; Install mozc_emacs_helper as well as mozc.el using Nix
(straight-use-package '(mozc :type built-in))

(use-package mozc
  :disabled t)

(use-package mozc-cand-posframe
  :disabled t
  :after mozc
  :if (posframe-workable-p)
  :config
  (setq mozc-candidate-style 'posframe))

(use-package mozc-temp
  :disabled t
  :commands (mozc-temp-convert mozc-temp-convert-dwim))

(provide 'setup-japanese-mozc)
