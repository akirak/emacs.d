(use-package corral
  :general
  ("M-9" 'corral-parentheses-forward)
  ;; ("M-[" 'corral-brackets-forward)
  ;; ("M-]" 'corral-brackets-backward)
  ;; ("M-{" 'corral-braces-forward)
  ;; ("M-}" 'corral-braces-backward)
  ;; ("M-\"" 'corral-double-quotes-forward)
  :custom
  (corral-preserve-point t))

(provide 'init-corral)
