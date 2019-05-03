(setq initial-buffer-choice nil    ; Inhibit the default splash screen
      ;; Set the startup buffer
      initial-buffer-choice (unless initial-buffer-choice
                              (cond
                               (debug-on-error (lambda ()
                                                 (get-buffer "*Messages*")))))
      ;; Configure the scratch buffer
      initial-major-mode 'org-mode
      initial-scratch-message nil)

(add-hook 'emacs-startup-hook
          (lambda ()
            (with-current-buffer "*scratch*"
              (insert (format "Emacs started at %s (initialised in %.1fs)\n"
                              (format-time-string "%F %R" after-init-time)
                              (float-time (time-subtract after-init-time
                                                         before-init-time)))))))

(provide 'setup-initial-buffer)
