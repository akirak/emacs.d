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
  :config
  (with-eval-after-load 'counsel
    (ivy-add-actions 'counsel-M-x
                     `(("h" ,(-compose 'helpful-command 'intern) "helpful"))))
  :general
  ([help ?.] 'helpful-at-point))

(provide 'setup-helpful)
