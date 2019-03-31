(use-package ffap
  :straight nil
  :general
  ("C-x 4 f" #'ffap-other-window
   "C-x 4 r" #'ffap-read-only-other-window))

(provide 'setup-ffap)
