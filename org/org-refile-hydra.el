;;; org-refile-hydra.el --- Hydra for org-refile -*- lexical-binding: t -*-

(require 'hydra)

(defun org-refile-hydra//format-path ()
  (substring-no-properties
   (org-format-outline-path (cons (buffer-name) (org-get-outline-path t))
                            nil nil " > ")))

(defun org-refile--get-last-location ()
  "Get the last refile location as a marker."
  (when-let ((pair (assoc (plist-get org-bookmark-names-plist :last-refile)
                          bookmark-alist))
             (orig-pos (point-marker)))
    (save-current-buffer
      (save-excursion
        (bookmark-handle-bookmark (cdr pair))
        (unless (equal (point-marker) orig-pos)
          (org-up-heading-safe)
          (point-marker))))))

(defun org-refile-hydra//last-refile-location ()
  (when-let ((marker (org-refile--get-last-location)))
    (with-current-buffer (marker-buffer marker)
      (goto-char marker)
      (point-marker))))

(defun org-agenda-refile-to-last-location ()
  (interactive)
  (unless (eq major-mode 'org-agenda-mode)
    (user-error "Not in org-agenda-mode"))
  (if-let ((marker (org-refile--get-last-location))
           (filename (buffer-file-name (marker-buffer marker)))
           (pos (marker-position marker)))
      (org-agenda-refile nil (list nil filename nil pos))
    (user-error "Cannot retrieve the last refile location")))

(defun org-refile-hydra//current-clock ()
  (when (and (org-clocking-p)
             org-clock-marker
             (buffer-live-p (org-clocking-buffer)))
    (with-current-buffer (marker-buffer org-clock-marker)
      (org-with-wide-buffer
       (org-refile-hydra//format-path)))))

(defun org-refile-same-buffer (arg)
  "org-refile to a heading in the same buffer."
  (interactive "P")
  (let ((org-refile-targets `((nil . ,(cl-typecase arg
                                        (number `(:level . ,arg))
                                        (t '(:maxlevel . 9)))))))
    (call-interactively #'org-refile)))

(defun org-refile-targets-other-window-files ()
  "org-refile to a buffer in a window in the same frame."
  (cl-loop for win in (cl-remove (selected-window) (window-list nil nil))
           with result = nil
           for buf = (window-buffer win)
           collect (with-current-buffer buf
                     (and (derived-mode-p 'org-mode)
                          (buffer-file-name)))
           into result
           finally return (mapcar (lambda (filename)
                                    `(,filename :maxlevel . 9))
                                  (cl-remove-duplicates
                                   (cl-remove nil result)
                                   :test 'equal))))

(defun org-refile-other-window-files ()
  "org-refile to a buffer in a window in the same frame."
  (interactive)
  (let ((org-refile-targets (org-refile-targets-other-window-files)))
    (org-refile)))

(defun org-agenda-refile-other-window-files ()
  "org-refile to a buffer in a window in the same frame."
  (interactive)
  (let ((org-refile-targets (org-refile-targets-other-window-files)))
    (org-agenda-refile)))

(defun org-journal-refile-location (time)
  "Get a refile location in org-journal at/on TIME."
  (save-window-excursion
    (org-with-wide-buffer
     (org-journal-new-entry t time)
     (goto-char (point-min))
     (unless (string-match "^\*" (thing-at-point 'line t))
       (re-search-forward "^\* "))
     (let ((filename (buffer-file-name)))
       (message "Refiling to %s" filename)
       (list nil filename nil (point))))))

(defun org-journal-entry-time (arg)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in org-mode"))
  (if arg
      (let* ((date (org-read-date))
             (time (org-time-string-to-time date)))
        (when (time-less-p (current-time) time)
          (org-set-property "SCHEDULED" date)
          (org-todo "TODO"))
        time)
    (or (when-let ((time-string (or (org-entry-get nil "CLOSED")
                                    (org-entry-get nil "SCHEDULED"))))
          (org-time-string-to-time time-string))
        (current-time))))

(defun org-refile-to-journal (arg)
  "Refile the current entry to org-journal."
  (interactive "P")
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in org-mode"))
  (when (org-before-first-heading-p)
    (user-error "Before first heading"))
  (when (region-active-p)
    (user-error "Cannot be run on a region"))
  (org-refile nil nil (org-journal-refile-location (org-journal-entry-time arg))))

(defun org-agenda-refile-to-journal (arg)
  "Refile the current entry to org-journal."
  (interactive "P")
  (unless (eq major-mode 'org-agenda-mode)
    (user-error "Not in org-agenda-mode"))
  (let* ((buffer-orig (buffer-name))
         (marker (or (org-get-at-bol 'org-hd-marker)
                     (org-agenda-error)))
         (rfloc (with-current-buffer (marker-buffer marker)
                  (goto-char (marker-position marker))
                  (org-journal-refile-location (org-journal-entry-time arg)))))
    (org-agenda-refile nil rfloc)))

(defhydra org-refile-hydra
  (:hint nil)
  "
org-refile
_l_: last refile: %s(org-refile-hydra//last-refile-location)
_c_: clock: %s(org-refile-hydra//current-clock)
"
  ("l" (org-refile '(16)))
  ("c" (org-refile 2))
  ("w" org-refile "normal")
  ("a" avy-org-refile-as-child "avy")
  ("b" org-refile-same-buffer "in-buffer")
  ("v" org-refile-other-window-files "other windows")
  ("j" org-refile-to-journal "org-journal")
  ("u" universal-argument "C-u")
  ("p" org-previous-visible-heading "previous")
  ("n" org-next-visible-heading "next")
  ("q" nil "quit" :exit t))

(defhydra org-agend-refile-hydra
  (:hint nil)
  "
org-refile
_l_: last refile: %s(org-refile-hydra//last-refile-location)
"
  ("l" org-agenda-refile-to-last-location)
  ("w" org-agenda-refile "normal")
  ("v" org-agenda-refile-other-window-files "other windows")
  ("j" org-agenda-refile-to-journal "org-journal")
  ("u" universal-argument "C-u")
  ("p" org-agenda-previous-line "previous")
  ("n" org-agenda-next-line "next")
  ("q" nil "quit" :exit t))

(defun org-refile-hydra ()
  (interactive)
  (bookmark-maybe-load-default-file)
  (cond
   ((derived-mode-p 'org-mode) (org-refile-hydra/body))
   ((eq major-mode 'org-agenda-mode) (org-agenda-refile-hydra/body))
   (t (user-error "Not in org mode"))))

(provide 'org-refile-hydra)
;;; org-refile-hydra.el ends here
