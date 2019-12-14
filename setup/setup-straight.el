;; Extra functions for straight.el

(defvar akirak/straight-rebuilt-outdated-packages nil)

(defun akirak/straight-get-outdated-packages ()
  (with-current-buffer "*Messages*"
    (let (result)
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward (rx bol "Source file ‘"
                                      (group (+? anything))
                                      "’ newer than byte-compiled file" eol)
                                  nil t)
          (let* ((fpath (buffer-substring-no-properties
                         (match-beginning 1)
                         (match-end 1)))
                 (package (when (string-match (rx (group (+ (not (any "/"))))
                                                  "/" (+ (not (any "/"))) eol)
                                              fpath)
                            (match-string 1 fpath))))
            (push package result))))
      (set-difference (cl-remove-duplicates (nreverse result)
                                            :test #'string-equal)
                      akirak/straight-rebuilt-outdated-packages))))

(defun akirak/straight-rebuild-outdated-packages (&optional reload)
  "Rebuild outdated packages."
  (interactive "P")
  (let* ((packages (akirak/straight-get-outdated-packages))
         (total (length packages))
         (i 1)
         (start-time (current-time))
         finish-time
         package)
    (while (setq package (pop packages))
      (message "Rebuilding packages %s (%d/%d)..."
               package i total)
      (let (message-log-max)
        (straight-rebuild-package package))
      (push package akirak/straight-rebuilt-outdated-packages)
      (when reload
        (load package))
      (setq i (1+ i)))
    (setq finish-time (current-time))
    (garbage-collect)
    (message "Finished rebuilding %d package in %.1f seconds."
             total (float-time (time-subtract finish-time start-time)))))

;; After an idle of 10 minutes, rebuild the outdated packages.
(run-with-idle-timer 600 nil #'akirak/straight-rebuild-outdated-packages)

(provide 'setup-straight)
