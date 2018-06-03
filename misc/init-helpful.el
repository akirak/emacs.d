(use-package helpful
  :commands (helpful-function
             helpful-variable
             helpful-at-point
             helpful-command)
  :config
  (with-eval-after-load 'counsel
    (cl-loop for (command . action) in '((counsel-describe-function . helpful-function)
                                         (counsel-describe-variable . helpful-variable)
                                         (counsel-M-x . helpful-command))
             do (ivy-add-actions command
                                 `(("f" ,(-compose action 'intern) "helpful")))))
  ;; :general
  ;; ([help ?f] 'helpful-function
  ;;  [help ?v] 'helpful-variable
  ;;  [help ?\.] 'helpful-at-point)
  )

(provide 'init-helpful)
