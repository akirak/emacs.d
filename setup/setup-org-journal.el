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

  (defun akirak/org-journal-cleanup-empty-files ()
    (interactive)
    (cl-labels ((is-empty-journal-file-p
                 (file)
                 (and (not (find-buffer-visiting file))
                      (with-temp-buffer
                        (insert-file-contents file)
                        (goto-char (point-min))
                        (let ((case-fold-search t))
                          (catch 'nonempty
                            (while (< (point) (point-max))
                              (unless (looking-at (rx bol (or eol "* " "#+title:")))
                                (throw 'nonempty nil))
                              (forward-line))
                            t))))))
      (let ((files (->> (akirak/org-journal-files)
                        (-filter #'is-empty-journal-file-p))))
        (mapc #'move-file-to-trash files)
        (when files
          (message "Moved %d files to the trash: %s"
                   (length files)
                   (mapconcat #'file-name-nondirectory files " "))))))

  ;; To configure org-journal, you should call one of the following
  ;; functions and set `org-journal-dir'.
  ;;
  ;; I'll use daily journal for personal use, and weekly for
  ;; workplace.

  (defmacro akirak/org-journal-todo-match-expr (&rest todos)
    "Used to set `org-journal-carryover-items' variable."
    (concat "TODO="
            (cl-etypecase todos
              (string (format "\"%s\"" todos))
              (list (format "\{%s\}"
                            (string-join todos "\\|"))))))

  (defun akirak/setup-daily-org-journal ()
    (setq org-journal-file-type 'daily))

  (defun akirak/setup-weekly-org-journal ()
    ;; (setq org-extend-today-until 4
    ;;       org-journal-carryover-items "TODO=\{TODO\\|NEXT\\|STARTED\}")
    (setq org-journal-file-type 'weekly
          org-journal-file-header
          (defun akirak/org-journal-weekly-header (time)
            (format-time-string "#+TITLE: Week %-W, %Y" time))))

  (org-starter-def-capture "j" "Journal")
  (org-starter-def-capture "jj" "Journal - Plain entry"
    entry (function org-journal-find-location)
    "** %?
:PROPERTIES:
:CREATED_TIME: %U
:END:
" :clock-in t :clock-resume t :unnarrowed t)
  (org-starter-def-capture "jt" "Journal - Todo entry"
    entry (function org-journal-find-location)
    "** TODO %?
:PROPERTIES:
:CREATED_TIME: %U
:END:
" :clock-in t :clock-resume t :unnarrowed t)

  :custom
  (org-journal-date-format "%F (%a)")
  (org-journal-file-format "%Y%m%d.org"))

(provide 'setup-org-journal)
