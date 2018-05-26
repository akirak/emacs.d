(define-prefix-command 'akirak/frame-map)

(general-def akirak/frame-map
  "w" '((lambda () (interactive)
          (frame-workflow-switch-frame 'web t)) :which-key "web")
  "e" '((lambda () (interactive)
          (frame-workflow-switch-frame 'emacs-lisp t)) :which-key "emacs-lisp"))

(provide 'init-frame-map)
