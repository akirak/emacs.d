;;; init-org-agenda.el --- Utilities for org-agenda -*- lexical-binding: t -*-

;;;;  Automatically generating agenda-group

(defun akirak/update-agenda-group-1 ()
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in org-mode"))
  (org-with-wide-buffer
   (goto-char (point-min))
   (while (re-search-forward "^\* " nil t)
     (org-entry-put nil "agenda-group" (substring-no-properties (org-get-heading t t t t))))))

(provide 'init-org-agenda)
;;; init-org-agenda.el ends here
