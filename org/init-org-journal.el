(use-package org-journal
  :config
  (defun org-journal-find-location ()
    "Go to the beginning of the today's journal file.

This can be used for an org-capture template to create an entry in the journal."
    ;; Open today's journal, but specify a non-nil prefix argument in order to
    ;; inhibit inserting the heading; org-capture will insert the heading.
    (org-journal-new-entry t)
    ;; Position point on the journal's top-level heading so that org-capture
    ;; will add the new entry as a child entry.
    (goto-char (point-min)))
  (general-unbind :keymaps 'org-journal-mode-map "C-c C-s")
  (general-unbind "C-c C-j")
  :custom
  (org-extend-today-until 4)
  (org-journal-date-format "%F (%a)")
  (org-journal-enable-agenda-integration t "Add future journal files to agenda")
  (org-journal-carryover-items "TODO=\{TODO\\|NEXT\}"))

;;;; Add org-journal to org-refile-targets

(defun org-journal-new-entry-file (&optional time)
  (save-window-excursion
    (org-journal-new-entry t time)
    (buffer-file-name)))

;; Add the current file of org-journal to org-refile-targets.
;; Currently disabled.
;; (with-eval-after-load 'org
;;   (add-to-list 'org-refile-targets
;;                '(org-journal-new-entry-file . (:level . 1))))

(provide 'init-org-journal)
