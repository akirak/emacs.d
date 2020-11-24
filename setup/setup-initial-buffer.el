(setq initial-buffer-choice
      (when (and (not initial-buffer-choice)
                 init-file-debug)
        (lambda () (get-buffer "*Messages*"))))

;; Prevent from initializing lisp-interaction-mode
(setq initial-major-mode 'text-mode
      initial-scratch-message "")

(provide 'setup-initial-buffer)
