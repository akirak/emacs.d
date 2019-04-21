(use-package perfect-margin
  :straight (perfect-margin :host github :repo "mpwang/perfect-margin")
  :config
  (defun akirak/exwm-window-p (window)
    ;; It seems that a frame is sometimes passed to this function,
    ;; so I have to guard it
    (and (windowp window)
         (eq 'exwm-mode
             (buffer-local-value 'major-mode (window-buffer window)))))
  (add-to-list 'perfect-margin-ignore-filters 'akirak/exwm-window-p)
  (perfect-margin-mode 1)
  :custom
  (perfect-margin-visible-width 92)
  (perfect-margin-ignore-regexps `("^minibuf" "^[*]"
                                   "^ \\*LV\\*"
                                   "^ \\*which-key\\*")))

(provide 'setup-perfect-margin)
