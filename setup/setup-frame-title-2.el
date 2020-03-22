(setq-default frame-title-format
              `(,(if (akirak/exwm-session-p) "EXWM@" "Emacs@")
                ,(string-trim-right (shell-command-to-string "uname -n"))))

(provide 'setup-frame-title-2)
