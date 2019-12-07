;; Extra functions for straight.el

(defvar akirak/straight-rebuilt-outdated-packages nil)

(defun akirak/straight-rebuild-outdated-packages (&optional reload)
  "FIXME"
  (interactive "P")
  (let ((packages (with-current-buffer "*Messages*"
                    (save-excursion
                      (goto-char (point-min))
                      (let (result
                            (prefix (expand-file-name (locate-user-emacs-file "straight/build/"))))
                        (while (re-search-forward (rx "Source file ‘"
                                                      (group (+? anything))
                                                      "’ newer than byte-compiled file")
                                                  nil t)
                          (let* ((data (match-data 1))
                                 (filepath (buffer-substring (nth 2 data) (nth 3 data))))
                            (when (string-prefix-p prefix filepath)
                              (let ((package (string-remove-suffix
                                              "/"
                                              (file-name-directory
                                               (string-remove-prefix prefix filepath)))))
                                (unless (member package akirak/straight-rebuilt-outdated-packages)
                                  (push package result))))))
                        result)))))
    (dolist (package (cl-remove-duplicates packages :test #'string-equal))
      (straight-rebuild-package package)
      (when reload
        (load package))
      (push package akirak/straight-rebuilt-outdated-packages))))

;; During the first idle of 5 minutes, rebuild the outdated packages.
(run-with-idle-timer 300 nil #'straight-check-all)

(provide 'setup-straight)
