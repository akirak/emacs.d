(use-package counsel
  ;; :diminish counsel-mode
  :config
  (counsel-mode 1) ; Remap built-in functions with counsel equivalents
  (ivy-add-actions #'counsel-find-library
                   '(("l" load-library "load")))
  (cl-loop for (command find-other-window)
           in '((counsel-describe-function find-function-other-window)
                (counsel-describe-variable find-variable-other-window)
                (counsel-M-x find-function-other-window))
           do (ivy-add-actions command
                               `(("j" ,(-compose find-other-window 'intern)
                                  "definition in other window"))))
  (global-set-key [remap recentf-open-files] 'counsel-recentf)
  (global-set-key [remap insert-char] 'counsel-unicode-char))

(defun akirak/ad-after-counsel-org-goto-action (_x)
  (org-show-entry))
(advice-add 'counsel-org-goto-action :after
            'akirak/ad-after-counsel-org-goto-action)

(provide 'setup-counsel)
