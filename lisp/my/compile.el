(defvar akirak/compile-history-alist nil)

(defvar akirak/compile-nix-shell-args nil)

(cl-defun akirak/compile (command &rest args)
  "Wrap the command."
  (cl-check-type command string)
  (cl-check-type args list)
  (-let [(&plist :nix-shell-args :directory) args]
    (cl-check-type nix-shell-args (or null list))
    (cl-check-type directory (or null file-directory))
    (let* ((nix-shell-args (or nix-shell-args akirak/compile-nix-shell-args))
           (command (if nix-shell-args
                        (concat "nix-shell "
                                (mapconcat #'shell-quote-argument
                                           nix-shell-args " ")
                                " --command "
                                (shell-quote-argument command))
                      command))
           (directory (or directory default-directory))
           ;; The command must be dependent on the execution directory
           (entry (cons command
                        (if-let ((cell (member :directory args)))
                            (progn
                              (setcdr cell (cons directory (cddr cell)))
                              args)
                          (append `(:directory ,directory) args))))
           ;; Set the directory for compilation command
           (default-directory directory))
      ;; Make the given command appear first in the history
      (cl-delete entry akirak/compile-history-alist)
      (push entry akirak/compile-history-alist)
      (compile command))))

(defun akirak/format-compile-history-entry (cell)
  (-let* (((command . args) cell)
          ((&plist :nix-shell-args :directory) args))
    (format "%s [%s]" command directory)))

(defun akirak/run-command-in-vterm-as-compile (command)
  (let* ((project (f-filename default-directory))
         (buffer (format "*VTerm:%s: %s*" project command)))
    (if (and (get-buffer buffer)
             (not (yes-or-no-p (format "%s is already running. Kill it and start the command again?"
                                       buffer))))
        (display-buffer buffer)
      (akirak/run-interactive-shell-command command
        buffer))))

(provide 'my/compile)
