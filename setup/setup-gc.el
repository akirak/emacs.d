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
  (defun akirak/gcmh-save-log ()
    "Save the gcmh log to a file."
    (ignore-errors
      (when (require 'xdg nil t)
        (message "Saving gcmhg log...")
        (let ((out-dir (expand-file-name "emacs" (xdg-data-home))))
          (unless (file-directory-p out-dir)
            (make-directory out-dir))
          (with-current-buffer gcmh-log-buffer
            (write-file (expand-file-name
                         (format-time-string "emacs-gcmh-%s.log")
                         out-dir)))))))
  :hook
  Save the gcmhg log for benchmarking.
  (kill-emacs . akirak/gcmh-save-log)
  :custom
  (gcmh-verbose t)
  (gcmh-idle-delay 15))

(provide 'setup-gc)
