;; The following settings should be done in individual hosts:
;;
;; - Setting `org-journal-enable-agenda-integration' to t
;; - Setting `org-journal-dir' using `general-setq'

(use-package org-journal
  :after org-starter
  :config
  (defun org-journal-find-location ()
    "Go to the beginning of the today's journal file.

This can be used for an org-capture template to create an entry in the journal."
    ;; Open today's journal, but specify a non-nil prefix argument in order to
    ;; inhibit inserting the heading; org-capture will insert the heading.
    (org-journal-new-entry t)
    ;; Position point on the journal's top-level heading so that org-capture
    ;; will add the new entry as a child entry.
    (widen)
    (goto-char (point-min))
    (org-show-entry))
  (defun akirak/org-journal-open-today ()
    (interactive)
    (org-journal-new-entry t))
  (defun akirak/org-journal-files ()
    (directory-files org-journal-dir t
                     (file-name-nondirectory org-journal-file-pattern)))
  (defun akirak/helm-org-rifle-org-journal ()
    (interactive)
    (helm-org-rifle-files (nreverse (akirak/org-journal-files))))
  (add-to-list 'org-starter-extra-find-file-map
               '("J" akirak/org-journal-open-today "org-journal"))
  (add-to-list 'org-starter-extra-find-file-map
               '("C-j" org-journal-new-scheduled-entry "org-journal (schedule)"))
  (add-to-list 'org-starter-extra-alternative-find-file-map
               '("J" akirak/helm-org-rifle-org-journal "org-journal"))
  (org-starter-def-capture "J" "org-journal (plain)"
    entry (function org-journal-find-location)
    "* %?
:PROPERTIES:
:CREATED_TIME: %U
:END:
"
    :unnarrowed t :clock-in t :clock-resume t)
  ;; Don't bind C-c C-j to org-journal-new-entry
  (general-unbind "C-c C-j")
  :custom
  (org-extend-today-until 4)
  (org-journal-carryover-items "TODO=\{TODO\\|NEXT\\|STARTED\}")
  (org-journal-date-format "%F (%a)"))

(provide 'setup-org-journal)
