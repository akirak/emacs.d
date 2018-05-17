(require 'hydra)

(defhydra akirak/emacs-lisp-hydra
  (:hint nil)
  "
^^Eval             ^^Check            ^^Toggle
^^--------------   ^^---------------  ^^------------------------
_e_: eval-buffer   _l_: package-lint  _td_: debug-on-error [%s(if debug-on-error \"on\" \"off\")]
_me_: macroexpand

"
  ("e" eval-buffer :exit t)
  ("me" emacs-lisp-macroexpand :exit t)
  ("l" package-lint-current-buffer :exit t)
  ("td" toggle-debug-on-error))

;; Use this alias to run the hydra so that the hydra can become
;; context-sensitive in the future.
(defalias 'akirak/emacs-lisp-hydra 'akirak/emacs-lisp-hydra/body)

(provide 'emacs-lisp-hydra)
