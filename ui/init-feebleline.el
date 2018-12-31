(use-package feebleline
  :straight (feebleline :host github :repo "tautologyclub/feebleline"
                        :branch "development")
  :init
  (feebleline-mode 1)
  :config
  (setq feebleline-msg-functions
        '(((lambda () (format-time-string "%b %d %a %H:%M")) :post " " :face font-lock-comment-face)
          (feebleline-file-directory :face feebleline-dir-face :post "")
          (feebleline-file-or-buffer-name :face font-lock-keyword-face :post " ")
          ((lambda () (when (and buffer-file-name (require 'magit nil t))
                        (magit-get-current-branch))) :face font-lock-string-face :post " ")
          ((lambda () mode-name) :post " " :face font-lock-comment-face)
          ((lambda () (format-mode-line "%I")) :post " " :face font-lock-comment-face)
          (akirak/org-clock-summary :face font-lock-builtin-face :pre " :: ")
          ((lambda () (frame-parameter nil 'name))
           :align right :face font-lock-function-name-face))))

(defun akirak/org-clock-summary ()
  (when (org-clocking-p)
    (concat (let* ((duration (time-subtract (current-time)
                                            org-clock-start-time))
                   (seconds (float-time duration)))
              (cond
               ((< seconds 3600) (format-time-string "%-M:%S" duration))
               ((< seconds 86400) (format-time-string "%-Hh %-Mm" duration))
               (t (org-minutes-to-clocksum-string
                   (floor seconds 60)))))
            " on "
            (or org-clock-current-task
                (with-current-buffer (marker-buffer org-clock-marker)
                  (org-with-wide-buffer
                   (goto-char org-clock-marker)
                   (nth 4 (org-heading-components))))
                "-")
            " (in "
            (buffer-name (marker-buffer org-clock-marker))
            ")")))

(provide 'init-feebleline)
