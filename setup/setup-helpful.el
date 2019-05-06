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
  ([help ?.] 'helpful-at-point)
  :custom
  (helpful-switch-buffer-function 'akirak/helpful-switch-buffer))

(defun akirak/helpful-switch-buffer (&rest args)
  "My `helpful-switch-buffer-function' function.

If the current buffer is in `helpful-mode', switch to the buffer
in the same window.  Otherwise, display the buffer in other window
and switch to it.  This behavior can be overridden by customizing
`display-buffer-alist'."
  (if (eq major-mode 'helpful-mode)
      (apply #'pop-to-buffer-same-window args)
    (apply #'pop-to-buffer args)))

(provide 'setup-helpful)
