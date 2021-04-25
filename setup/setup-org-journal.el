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

  (defun akirak/org-journal-cleanup-empty-dates ()
    (interactive)
    (cl-labels ((clean-empty-toplevel-heading
                 ()
                 (and (looking-at (rx "*" (+ space) (+ not-newline) "\n"
                                      (* space) ":PROPERTIES:\n"
                                      (* space) ":CREATED:"
                                      (* space) (+ (any digit))
                                      (* space) "\n"
                                      (* space) ":END:"))
                      (goto-char (nth 1 (match-data)))
                      (let ((beg (point)))
                        (org-end-of-subtree)
                        (when (string-match-p (rx bos (* space) eos)
                                              (buffer-substring-no-properties beg point))
                          (delete-region beg (point))
                          t))))
                (is-empty-journal-file-p
                 (file)
                 (and (not (find-buffer-visiting file))
                      (with-temp-buffer
                        (insert-file-contents file)
                        (delay-mode-hooks 'org-mode)
                        (goto-char (point-min))
                        (let ((case-fold-search t))
                          ;; Skip the file header
                          (when (looking-at (rx "#+title: " (+ nonl)))
                            (goto-char (nth 1 (match-data))))
                          ;; Skip empty lines and delete empty dates
                          ;; (with possible buffer modifications)
                          (catch 'nonempty
                            (while (< (point) (point-max))
                              (unless (or (looking-at (rx bol eol))
                                          (clean-empty-toplevel-heading))
                                (throw 'nonempty nil))
                              (forward-line))
                            (when (buffer-modified-p)
                              (save-buffer))
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

  (org-starter-def-capture "j" "Journal")
  (org-starter-def-capture "jj" "Journal - Plain entry"
    entry (function org-journal-find-location)
    "** %?
:PROPERTIES:
:CREATED_TIME: %U
:END:
" :clock-in t :clock-resume t)
  (org-starter-def-capture "jn" "Journal - Next entry"
    entry (function org-journal-find-location)
    "** NEXT %?
:PROPERTIES:
:CREATED_TIME: %U
:END:
" :clock-in t :clock-resume t)
  (org-starter-def-capture "jt" "Journal - Todo entry"
    entry (function org-journal-find-location)
    "** TODO %?
:PROPERTIES:
:CREATED_TIME: %U
:END:
" :clock-in t :clock-resume t)
  (org-starter-def-capture "js" "Journal - Start working on a task immediately")
  (org-starter-def-capture "jss" "Start a task with a heading"
    entry #'org-journal-find-location
    "** STARTED %?
:PROPERTIES:
:CREATED_TIME: %U
:END:
" :clock-in t :clock-resume t)
  (org-starter-def-capture "jsh" "Start a task with the selected text as a heading"
    entry (function org-journal-find-location)
    "** STARTED %i
:PROPERTIES:
:CREATED_TIME: %U
:END:
%?" :clock-in t :clock-resume t)
  (org-starter-def-capture "jsl" "Start a task with a link to the current position (immediate finish)"
    entry (function org-journal-find-location)
    "** STARTED %^{Title}
:PROPERTIES:
:CREATED_TIME: %U
:END:

%

%?" :clock-in t :clock-resume t)
  (push '("jsh" (region-active-p)) org-capture-templates-contexts)
  (org-starter-def-capture "jl" "Journal - With link")
  (org-starter-def-capture "jlc" "(generic) Comment on the entry"
    entry (function org-journal-find-location)
    ;; You can use `org-edit-headline' to edit the headline quickly
    "** %^{Heading prefix|Comment on } %a
:PROPERTIES:
:CREATED_TIME: %U
:END:

%a

%?
" :clock-in t :clock-resume t)
  (org-starter-def-capture "jld" "Log done"
    entry (function org-journal-find-location)
    "** Finished %a :@done:
:PROPERTIES:
:CREATED_TIME: %U
:END:

%?
" :clock-in t :clock-resume t)
  (org-starter-def-capture "jlp" "Log progress"
    entry (function org-journal-find-location)
    "** Progress on %a :@progress:
:PROPERTIES:
:CREATED_TIME: %U
:END:

%?
" :clock-in t :clock-resume t)

  (cl-defun akirak/org-capture-template-to-clock (&key extra-tags)
    (assert (org-clocking-p))
    (let ((marker (or org-clock-hd-marker org-clock-marker)))
      (with-current-buffer (marker-buffer marker)
        (goto-char marker)
        (let ((tags (let ((tags (append (org-get-tags-at)
                                        extra-tags)))
                      (if tags
                          (concat ":" (string-join tags ":") ":")
                        ""))))
          (substring-no-properties (format "** %%K %s
:PROPERTIES:
:CREATED_TIME: %%U
:CATEGORY: %s
:END:

%%?
"
                                           tags
                                           (org-get-category)))))))

  (org-starter-def-capture "l" "Logging")
  (org-starter-def-capture "ld" "done"
    entry (function org-journal-find-location)
    (function (lambda () (akirak/org-capture-template-to-clock
                          :extra-tags '("@done")))))
  (add-to-list 'org-capture-templates-contexts
               '("ld" (org-clocking-p))
               t (-on #'equal #'car))

  (add-hook 'org-journal-mode-hook
            (defun akirak/org-journal-entry-init ()
              (setq-local org-multi-wiki-want-custom-id nil)))

  :custom
  (org-journal-find-file #'find-file)
  (org-journal-date-format "%F (%a)")
  (org-journal-file-format "%Y%m%d.org"))

(provide 'setup-org-journal)
