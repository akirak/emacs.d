(require 'hydra)

(defhydra akirak/emacs-lisp-hydra
  (:hint t)
  "
^^Eval            ^^Check            ^^Toggle
^^--------------  ^^---------------  ^^------------------------
_e_: eval-buffer  _l_: package-lint  _td_: debug-on-error [%s(if debug-on-error \"on\" \"off\")]

"
  ("e" eval-buffer :exit t)
  ("l" package-lint-current-buffer :exit t)
  ("td" toggle-debug-on-error "toggle-debug-on-error"))

(provide 'emacs-lisp-hydra)
