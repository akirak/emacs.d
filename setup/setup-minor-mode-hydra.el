(defun akirak/minor-mode-hydra-status (mode)
  (format "%s %-5s"
          (symbol-name mode)
          (format "[%s]" (if (bound-and-true-p mode) "on" "off"))))

(defhydra akirak/minor-mode-hydra (:exit nil)
  "
_d_ %s(akirak/minor-mode-hydra-status 'debug-on-error)
_f_ %s(akirak/minor-mode-hydra-status 'focus-mode)
_v_ %s(akirak/minor-mode-hydra-status 'view-mode)

_s_ %s(akirak/minor-mode-hydra-status 'smartparens-mode)
_S_ %s(akirak/minor-mode-hydra-status 'smartparens-strict-mode)
_l_ %s(akirak/minor-mode-hydra-status 'lispy-mode)
_p_ %s(akirak/minor-mode-hydra-status 'page-break-lines-mode)
_ed_ %s(akirak/minor-mode-hydra-status 'eldoc-mode)
_el_ %s(akirak/minor-mode-hydra-status 'electric-layout-mode)
_ei_ %s(akirak/minor-mode-hydra-status 'electric-indent-local-mode)
_eq_ %s(akirak/minor-mode-hydra-status 'elecric-quote-local-mode)

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
