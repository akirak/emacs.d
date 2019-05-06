(defun akirak/minor-mode-hydra-status (mode)
  (format "(%s) %s"
          (if (and (boundp mode) mode) "+" "-")
          (string-remove-suffix "-mode" (symbol-name mode))))

(defhydra akirak/minor-mode-hydra (:exit nil
                                         :hint nil
                                         :foreign-keys warn)
  "
[focus]
  _v_ %s(akirak/minor-mode-hydra-status 'view-mode)\
  _f_ %s(akirak/minor-mode-hydra-status 'focus-mode)
[info]
  _d_ %s(akirak/minor-mode-hydra-status 'debug-on-error)\
  _ed_ %s(akirak/minor-mode-hydra-status 'eldoc-mode)
[edit]
  _s_ %s(akirak/minor-mode-hydra-status 'smartparens-mode)\
  _S_ %s(akirak/minor-mode-hydra-status 'smartparens-strict-mode)\
  _l_ %s(akirak/minor-mode-hydra-status 'lispy-mode)
[assistance]
  _el_ %s(akirak/minor-mode-hydra-status 'electric-layout-mode)\
  _ei_ %s(akirak/minor-mode-hydra-status 'electric-indent-local-mode)\
  _eq_ %s(akirak/minor-mode-hydra-status 'elecric-quote-local-mode)
[other display]
  _p_ %s(akirak/minor-mode-hydra-status 'page-break-lines-mode)
"
  ("d" toggle-debug-on-error)
  ("l" lispy-mode)
  ("s" smartparens-mode)
  ("S" smartparens-strict-mode)
  ("f" focus-mode)
  ("v" view-mode)
  ("ed" eldoc-mode)
  ("p" page-break-lines-mode)
  ("el" electric-layout-mode)
  ("eq" electric-quote-local-mode)
  ("ei" electric-indent-local-mode)
  ("q" nil "quit" :exit t))

(provide 'setup-minor-mode-hydra)
