(use-package helpful
  :commands (helpful-function
             helpful-variable
             helpful-at-point
             helpful-command)
  :init
  ;; Configuration for using helpful with counsel.
  ;; From https://www.reddit.com/r/emacs/comments/8t3imt/helpful_one_year_on/e1eoy6v/
  (setq counsel-describe-variable-function 'helpful-variable
        counsel-describe-function-function 'helpful-callable)
  (akirak/bind-help-key :keymaps '(helpful-mode-map help-mode-map)
    "." #'helpful-at-point)
  :config
  (with-eval-after-load 'counsel
    (cl-loop for (command . action) in '((counsel-describe-function . helpful-function)
                                         (counsel-describe-variable . helpful-variable)
                                         (counsel-M-x . helpful-command))
             do (ivy-add-actions command
                                 `(("f" ,(-compose action 'intern) "helpful")))))
  :general
  ([help ?.] 'helpful-at-point))

(provide 'init-helpful)
