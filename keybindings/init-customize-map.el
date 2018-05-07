(define-prefix-command 'akirak/customize-map)

(general-def :keymaps 'akirak/customize-map
  "" '(nil :wk "customize")
  "f" #'customize-face-other-window
  "g" #'customize-group-other-window
  "l" #'load-library
  "p" '((lambda () (interactive)
          (if (featurep 'straight)
              (call-interactively 'straight-use-package)
            (package-list-packages)))
        :wk "packages")
  "s" #'customize-set-value
  "v" #'customize-variable-other-window
  "R" #'restart-emacs)

(provide 'init-customize-map)
