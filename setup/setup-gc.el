(use-package gcmh
  :straight (gcmh :host github :repo "akirak/gcmh"
                  :branch "logging")
  :config
  (defvar akirak/gcmh-status nil)
  (advice-add #'garbage-collect
              :before
              (defun akirak/gcmh-log-start (&rest _)
                (when gcmh-verbose
                  (setq akirak/gcmh-status "Running GC..."))))
  (advice-add #'gcmh-message
              :override
              (defun akirak/gcmh-message (format-string &rest args)
                (setq akirak/gcmh-status
                      (apply #'format-message format-string args))
                (run-with-timer 2 nil
                                (lambda ()
                                  (setq akirak/gcmh-status nil)))))
  (gcmh-mode 1)
  :custom
  (gcmh-verbose t)
  (gcmh-idle-delay 15))

(provide 'setup-gc)
