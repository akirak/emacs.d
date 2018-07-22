;;; init-org-agenda.el --- Utilities for org-agenda -*- lexical-binding: t -*-

;;;;  Automatically generating agenda-group

(defun akirak/update-agenda-group-1 ()
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in org-mode"))
  (org-with-wide-buffer
   (goto-char (point-min))
   (while (re-search-forward "^\* " nil t)
     (org-entry-put nil "agenda-group" (substring-no-properties (org-get-heading t t t t))))))

;;;; Define custom agenda commands

(defun akirak/org-add-agenda-custom-command (key desc &rest def)
  "Alternative to `org-add-agenda-custom-command' with extra features."
  ;; Check for an existing definition
  (declare (indent 2))
  (if-let ((current (assoc key org-agenda-custom-commands))
           (old-desc (nth 1 current)))
      ;; If it has the same description, override it
      (when (or (string-equal desc old-desc)
                ;; Otherwise, confirmation is needed
                (yes-or-no-p (format "Replace custom agenda command '%s' with '%s'?"
                                     old-desc desc)))
        (setcdr current (cons desc def)))
    (push `(,key ,desc ,@def) org-agenda-custom-commands)))

;;;; Utilities for customizing agenda

;;;;; Prefix formats

(defun akirak/org-agenda-format-prefix (&rest segments)
  "Render SEGMENTS."
  (save-excursion
    (org-back-to-heading t)
    (let ((element (org-element-at-point)))
      (mapconcat (lambda (segment)
                   (akirak/org-agenda-render-segment segment element))
                 segments " "))))

(defmacro akirak/org-agenda-prefix-format (&rest segments)
  "Define the value of a prefix format option in terms of SEGMENTS."
  `(format "  %%(akirak/org-agenda-format-prefix %s) "
           ,(mapconcat #'prin1-to-string segments " ")))

(defun akirak/org-agenda-render-segment (segment element)
  "Render SEGMENT of ELEMENT."
  (pcase segment
    ((pred stringp) segment)
    ((and `(,type . ,format)
          (guard (memq type '(scheduled deadline))))
     (let ((timestamp (case type
                        ('scheduled (org-element-property :scheduled element))
                        ('deadline (org-element-property :deadline element)))))
       (pcase format
         ('(date) (akirak/org-agenda-render-date timestamp))
         ('(date2) (akirak/org-agenda-render-date2 timestamp))
         ('(date-verbose) (akirak/org-agenda-render-date-verbosely timestamp))
         ('(date-convenient) (akirak/org-agenda-render-convenient-date timestamp))
         ('(relative-date-short) (akirak/org-agenda-render-relative-date-short timestamp))
         ('(time-of-day) (akirak/org-agenda-render-time-of-day timestamp)))))
    ('category (org-element-property :CATEGORY element))
    (`(property ,name)
     (org-element-property name element))
    (`(property ,name ,default)
     (or (org-element-property name element) default))
    ('repeat
     (if-let ((timestamp (org-element-property :scheduled element))
              (interval (org-element-property :repeater-value timestamp)))
         (format "%2dd" interval)
       "   "))))

;;;;;; Date and time formatting functions
(defun akirak/org-agenda-render-date-verbosely (timestamp)
  (concat (akirak/org-agenda-render-date timestamp)
          " "
          (akirak/org-agenda-render-date2 timestamp)))

(defun akirak/org-agenda-render-convenient-date (timestamp)
  (concat (if timestamp
              (format-time-string "%b %e"
                                  (org-time-string-to-time
                                   (org-element-property :raw-value timestamp)))
            (make-string 6 ?\ ))
          " " (akirak/org-agenda-render-date2 timestamp)))

(defun akirak/org-agenda-render-date (timestamp)
  (if timestamp
      (format-time-string "%F (%a)"
                          (org-time-string-to-time
                           (org-element-property :raw-value timestamp)))
    (make-string 16 ?\ )))

(defun akirak/org-agenda-render-date2 (timestamp)
  (format "%-3s"
          (if-let ((raw (org-element-property :raw-value timestamp))
                   (diff-days (org-time-stamp-to-now raw)))
              (cond
               ((< diff-days 100) (format "%dd" diff-days))
               ((< diff-days 365) (format "%dm" (/ diff-days 30)))
               (t (format "%dy" (/ diff-days 365))))
            "")))

(defun akirak/org-agenda-render-relative-date-short (timestamp)
  (if-let ((raw (org-element-property :raw-value timestamp))
           (diff-days (org-time-stamp-to-now raw)))
      (format "%2dd" diff-days)
    "   "))

(defun akirak/org-agenda-render-time-of-day (timestamp)
  (if-let ((hour (org-element-property :hour-start timestamp))
           (minute (org-element-property :minute-start timestamp)))
      (format "%2d:%02d" hour minute)
    "     "))

;;;;; Skip functions
(defun akirak/org-agenda-skip-recurring ()
  "Skip items with recurring schedule."
  (org-with-wide-buffer
   (let* ((element (org-element-at-point))
          (schedule (org-element-property :scheduled element)))
     (when (org-element-property :repeater-type schedule)
       (save-excursion
         (re-search-forward "^\\*" nil t))))))

(defun akirak/org-agenda-skip-but-level (level)
  (org-with-wide-buffer
   (let ((valid-level (org-get-valid-level level)))
     ;; Return nil if the current heading matches the level.
     (unless (eq (org-current-level) valid-level)
       (or (re-search-forward (concat "^" (regexp-quote (make-string valid-level ?*)) " ")
                              nil t)
           (point-max))))))

(defun akirak/org-agenda-skip-but-level-1 ()
  "Skip headings which is not at level 1."
  (akirak/org-agenda-skip-but-level 1))

(defun akirak/org-agenda-skip-but-level-2 ()
  "Skip headings which is not at level 2."
  (akirak/org-agenda-skip-but-level 2))

(provide 'init-org-agenda)
;;; init-org-agenda.el ends here
