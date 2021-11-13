;; The following settings should be done in individual hosts:
;;
;; - Setting `org-journal-enable-agenda-integration' to t
;; - Setting `org-journal-dir' using `general-setq'

(use-package org-journal
  :after org-starter
  :config
  (defun org-journal-find-location (&optional time)
    "Go to the beginning of the today's journal file.

This can be used for an org-capture template to create an entry in the journal."
    ;; Open today's journal, but specify a non-nil prefix argument in order to
    ;; inhibit inserting the heading; org-capture will insert the heading.
    (org-journal-new-entry t time)
    ;; Position point on the journal's top-level heading so that org-capture
    ;; will add the new entry as a child entry.
    (widen)
    (re-search-backward (rx bol "* "))
    (org-show-entry))
  (defun akirak/org-journal-open-today ()
    (interactive)
    (org-journal-new-entry t))
  (defalias 'akirak/org-journal-files 'org-journal--list-files)
  (defun akirak/helm-org-ql-journal ()
    (interactive)
    (helm-org-ql (nreverse (akirak/org-journal-files))))
  (add-to-list 'org-starter-extra-find-file-map
               '("j" akirak/org-journal-open-today "org-journal"))
  (add-to-list 'org-starter-extra-alternative-find-file-map
               '("j" akirak/helm-org-ql-journal "org-journal"))
  (general-unbind "C-c C-j")

  (akirak/bind-jump
    "M-d" #'org-journal-new-date-entry)

  (defun akirak/org-journal-refile-to-date (time)
    (interactive (list (org-read-date nil 'to-time)))
    (cond
     ((derived-mode-p 'org-mode)
      (let* ((orig-marker (prog1 (point-marker)
                            (org-journal-find-location time)))
             (filename (buffer-file-name))
             (rfloc (list (nth 4 (org-heading-components))
                          filename
                          nil
                          (point))))
        (with-current-buffer (marker-buffer orig-marker)
          (goto-char (marker-position orig-marker))
          (org-refile nil nil rfloc))))))

  (add-to-list 'org-starter-extra-refile-map
               '("j" akirak/org-journal-refile-to-date "journal")
               'append)

  ;; To configure org-journal, you should call one of the following
  ;; functions and set `org-journal-dir'.
  ;;
  ;; I'll use daily journal for personal use, and weekly for
  ;; workplace.

  (defun akirak/org-journal-todo-match-expr (todos)
    "Used to set `org-journal-carryover-items' variable."
    (concat "TODO="
            (cl-etypecase todos
              (string (format "\"%s\"" todos))
              (list (format "\{%s\}"
                            (string-join todos "\\|"))))))

  (cl-defun akirak/setup-org-journal (journal-dir daily-or-weekly
                                                  &key
                                                  carry-over)
    "Configure org-journal for the context."
    (declare (indent 1))
    (cl-ecase daily-or-weekly
      (weekly (setq org-journal-file-type 'weekly
                    org-journal-file-header
                    (defun akirak/org-journal-weekly-header (time)
                      (format-time-string "#+TITLE: Week %-W, %Y" time))))
      (daily (setq org-journal-file-type 'daily)))
    (setq org-journal-dir journal-dir
          org-journal-carryover-items (akirak/org-journal-todo-match-expr carry-over)
          org-journal-enable-agenda-integration t)
    ;; Update the current file for org-agenda.
    (akirak/org-journal-current-file)
    ;; Return the journal dir.
    journal-dir)

  (add-to-list 'recentf-exclude
               (defun akirak/org-journal-file-p (file)
                 (ignore-errors
                   (and (string-prefix-p (expand-file-name org-journal-dir)
                                         (expand-file-name file))
                        (string-suffix-p ".org" file))))
               t)

  (defun akirak/org-journal-current-file ()
    (ignore-errors
      (setq akirak/org-journal-current-file
            (org-journal--get-entry-path (current-time)))))

  (defvar akirak/org-journal-daily-timer nil)

  (defun akirak/org-journal-track-current-file ()
    "Update the current file every day."
    (interactive)
    (if akirak/org-journal-daily-timer
        (user-error "Timer already started")
      (setq akirak/org-journal-daily-timer
            (run-with-timer (let* ((now (current-time))
                                   (rollover (encode-time (append (list 0 0 4)
                                                                  (-drop 3 (decode-time now)))))
                                   (diff (- (float-time now) (float-time rollover))))
                              (if (> diff 0)
                                  (- 86400 diff)
                                (- diff)))
                            86400
                            #'akirak/org-journal-current-file))))

  (add-hook 'org-journal-mode-hook
            (defun akirak/org-journal-entry-init ()
              (setq-local org-multi-wiki-want-custom-id nil)))

  :general

  (:keymaps 'org-journal-mode-map
            ;; Use the same values as in org-mode-map
            "C-c C-f" #'org-forward-heading-same-level
            "C-c C-b" #'org-backward-heading-same-level)

  :custom
  (org-journal-find-file #'find-file)
  (org-journal-date-format "%F (%a)")
  (org-journal-file-format "%Y%m%d.org"))

(provide 'setup-org-journal)
