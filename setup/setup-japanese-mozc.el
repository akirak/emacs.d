;; Install mozc_emacs_helper as well as mozc.el using Nix
(straight-use-package '(mozc :type built-in))

(use-package mozc)

;; (find-library "mozc")

(use-package mozc-cand-posframe
  :after mozc
  :if (posframe-workable-p)
  :config
  (setq mozc-candidate-style 'posframe))

(use-package mozc-temp
  :disabled t
  :commands (mozc-temp-convert mozc-temp-convert-dwim))

(provide 'setup-japanese-mozc)
