(use-package uptimes
  :custom
  (uptimes-database "~/.emacs.d/.cache/uptimes.el"))
(use-package esup)

(defcustom akirak/init-time-log-file nil
  "Log file to record a history of `emacs-init-time'.")

(defun akirak/log-init-time ()
  "Record the Emacs initialization time to a text log file."
  (let ((log-file akirak/init-time-log-file)
        (init-time (emacs-init-time)))
    ;; Save to the log file if and only if its parent directory exists
    (when (and (stringp akirak/init-time-log-file)
               (file-directory-p (file-name-directory log-file)))
      (with-temp-buffer
        (insert (format-time-string "%FT%R%:z") ","
                (format "%s@%s" user-login-name (system-name)) ","
                init-time "\n")
        (append-to-file nil nil log-file)))
    (message "Emacs ready in %s with %d garbage collections."
             init-time gcs-done)))
(add-hook 'emacs-startup-hook 'akirak/log-init-time)

(defun akirak/view-init-log ()
  (interactive)
  (find-file-read-only akirak/init-time-log-file))

;;;; Startup window
;; Switch to *Messages* at startup
(defun akirak/setup-startup-windows ()
  (switch-to-buffer "*Messages*"))

(add-hook 'emacs-startup-hook 'akirak/setup-startup-windows)

(provide 'init-startup)
