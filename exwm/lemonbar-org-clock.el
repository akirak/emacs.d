(defvar lemonbar-org-clock-status nil)

(defvar lemonbar-org-clock-string nil)

(defvar lemonbar-org-clock-color nil)

(defconst lemonbar-org-clock-color-reset "{B-}{F-}")

(defcustom lemonbar-org-clock-clock-out-color
  "%{B\#3f51b5}%{F\#ffffff}"
  "Color format string applied when there is no running clock.")

(defcustom lemonbar-org-clock-default-clock-in-color
  "%{B\#8bc34a}%{F\#000000}"
  "Color format string applied when there is no running clock.")

(defcustom lemonbar-org-clock-offtime-clock-in-color
  "%{B\#e51c23}%{F\#ffffff}"
  "Color format string applied when the current clock is in offtime category.")

(defun lemonbar-org-clock--statistics (&optional start)
  "Get category statistics."
  (let* ((start (or start
                    (car (org-clock-special-range 'today))))
         (ents (cl-loop for fpath in (org-agenda-files)
                        append (with-current-buffer
                                   (or (find-buffer-visiting fpath)
                                       (find-file-noselect fpath))
                                 (org-map-entries
                                  (lambda ()
                                    (cons (org-get-category)
                                          (org-clock-sum-current-item
                                           start))))))))
    (setq lemonbar-org-clock-status
          (list (cons 'categories
                      (cl-loop for (category . log) in (seq-group-by 'car ents)
                               for total = (apply '+ (mapcar 'cdr log))
                               when (> total 0)
                               collect (cons category
                                             (org-minutes-to-clocksum-string total))))
                (cons 'total
                      (org-minutes-to-clocksum-string
                       (or (apply '+ (mapcar 'cdr ents))
                           0)))))))

(defun lemonbar-org-clock--current-clock ()
  "Summarize the clock information for when there is a running clock."
  (with-current-buffer (org-clocking-buffer)
    (org-with-wide-buffer
     (goto-char org-clock-marker)
     (let ((category (org-get-category))
           (title (nth 4 (org-heading-components)))
           (clocked-time (org-minutes-to-clocksum-string
                          (floor (- (float-time)
                                    (float-time org-clock-start-time)) 60)))
           (total org-clock-total-time)
           (effort org-clock-effort))
       (setq lemonbar-org-clock-color
             (if (string-equal category "offtime")
                 lemonbar-org-clock-offtime-clock-in-color
               lemonbar-org-clock-default-clock-in-color))
       (let-alist lemonbar-org-clock-status
         (format "%s on \"%s\" (Today: %s on %s, total %s)"
                 (concat clocked-time
                         (when (and total (> total 0))
                           (concat "+"
                                   (org-minutes-to-clocksum-string total)))
                         (when effort
                           (concat "/" (prin1-to-string effort))))
                 title
                 (or (cdr (assoc category .categories)) "0")
                 category
                 (or .total "0")))))))

(defvar lemonbar-org-clock-last-clock nil)

(defun lemonbar-org-clock--last-clock ()
  "Update information on the last running clock."
  (setq lemonbar-org-clock-last-clock
        (when-let ((marker (car org-clock-history)))
          (with-current-buffer (marker-buffer marker)
            (org-with-wide-buffer
             (goto-char marker)
             (list (cons 'title (nth 4 (org-heading-components)))
                   (cons 'time (org-clock-get-last-clock-out-time))
                   (cons 'category (org-get-category))))))))

(defun lemonbar-org-clock-update (&optional event trigger-update)
  "Update the string to describe the clock status."
  (when event
    (lemonbar-org-clock--statistics))
  (setq lemonbar-org-clock-string
        (if (org-clocking-p)
            (lemonbar-org-clock--current-clock)
          (setq lemonbar-org-clock-color lemonbar-org-clock-clock-out-color)
          (when (memq event '(clock-out start))
            (lemonbar-org-clock--last-clock))
          (let-alist lemonbar-org-clock-status
            (concat (format "Spent %s today" (or .total 0))
                    (when lemonbar-org-clock-last-clock
                      (let-alist lemonbar-org-clock-last-clock
                        (format ", clocked out at %s (%s ago) from \"%s\" in %s"
                                (format-time-string "%R" .time)
                                (org-minutes-to-clocksum-string
                                 (floor (- (float-time) (float-time .time)) 60))
                                .title
                                .category)))))))
  (when trigger-update (lemonbar-update)))

(when (require 'org-clock nil t)
  (add-hook 'org-clock-in-hook (lambda () (lemonbar-org-clock-update 'clock-in t)))
  (add-hook 'org-clock-out-hook (lambda () (lemonbar-org-clock-update 'clock-out t)))
  (add-hook 'lemonbar-start-hook (lambda () (lemonbar-org-clock-update 'start t)))
  (add-hook 'lemonbar-before-update-hook 'lemonbar-org-clock-update))

(provide 'lemonbar-org-clock)
