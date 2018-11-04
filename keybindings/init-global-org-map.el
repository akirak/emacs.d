(akirak/bind-global-org-map
  "a" #'org-agenda
  "b" #'ivy-switch-to-org-buffer
  "c" #'counsel-org-clock-context
  "f" #'helm-org-clock-follow-link
  "h" #'counsel-org-clock-history
  "j" #'akirak/pop-up-org-clocking-task
  "m" #'counsel-org-bookmark
  "n" #'org-clock-in-next-sibling
  "o" #'org-clock-out
  "q" #'org-clock-cancel
  "r" #'org-recent-headings
  "s" #'helm-org-rifle-agenda-files
  "t" #'yankpad-edit
  "u" #'org-clock-in-parent
  "v" #'org-web-tools-read-url-as-org
  "w" #'org-select-window
  "y" #'helm-org-clock-yank
  "L" #'org-starter-load-all-known-files)

(defun ivy-switch-to-org-buffer ()
  (interactive)
  (ivy-read "Org buffer: "
            (cl-remove-if-not
             (lambda (bufname)
               (with-current-buffer (get-buffer bufname)
                 (derived-mode-p 'org-mode)))
             (internal-complete-buffer "" nil t))
            :caller #'ivy-switch-buffer
            :action #'switch-to-buffer))

(defvar org-select-window-last-window nil)

(defun org-select-window (arg)
  (interactive "P")
  (if arg
      (progn
        (when org-select-window-last-window
          (select-window org-select-window-last-window)
          (setq org-select-window-last-window nil)))
    (let* ((wlist (window-list))
           (i0 (-elem-index (selected-window) wlist))
           (queue (append (-slice wlist (1+ i0))
                          (-take i0 wlist)))
           (w (-find (lambda (w)
                       (with-current-buffer (window-buffer w)
                         (derived-mode-p 'org-mode)))
                     queue)))
      (if w
          (progn
            (unless (derived-mode-p 'org-mode)
              (setq org-select-window-last-window (selected-window)))
            (select-window w))
        (message "No other org window in this frame")))))

(defun helm-org-clock-yank ()
  (interactive)
  (unless (org-clocking-p)
    (user-error "No running clock"))
  (let ((content (with-current-buffer (org-clocking-buffer)
                   (let (start end)
                     (org-with-wide-buffer
                      (goto-char org-clock-marker)
                      (setq end (save-excursion
                                  (org-end-of-subtree)
                                  (point)))
                      (forward-line)
                      (setq start (point))
                      (split-string (buffer-substring start end) "\n"))))))
    (helm :sources (helm-build-sync-source
                       (format "Content inside %s" org-clock-current-task)
                     :candidates
                     (--map-indexed
                      (cons (format "%d: %s" it-index it)
                            (cons it-index it))
                      content)))))

(defun helm-org-clock-follow-link ()
  (interactive)
  (unless (org-clocking-p)
    (user-error "No running clock"))
  (helm-org-follow-link--with-marker org-clock-marker))

(defun helm-org-follow-link--with-marker (marker)
  (let (begin end links root-path heading)
    (with-current-buffer (marker-buffer marker)
      (org-with-wide-buffer
       (goto-char marker)
       (org-back-to-heading)
       (setq root-path (org-get-outline-path t)
             heading (nth 4 (org-heading-components))
             begin (point)
             end (save-excursion
                   (org-end-of-subtree)
                   (point)))
       (while (re-search-forward org-bracket-link-regexp end t)
         (push (list (match-string-no-properties 1)
                     (match-string-no-properties 2)
                     (org-get-outline-path t t))
               links))))
    (if links
        (helm :sources
              (helm-build-sync-source (format "Links in %s: " heading)
                :candidates
                (mapcar
                 (pcase-lambda (`(,link ,title ,outline))
                   (cons (format "%s %s %s"
                                 (propertize (org-format-outline-path (seq-drop outline (length root-path)))
                                             'face 'font-lock-builtin-face)
                                 title
                                 (propertize link
                                             'face 'font-lock-comment-face))
                         link))
                 (nreverse links))
                :action
                (lambda (_)
                  (mapc #'org-open-link-from-string
                        (helm-marked-candidates)))))
      (message "No links"))))

(defmacro org-clock--with-clock (&rest progn)
  `(with-current-buffer (marker-buffer org-clock-marker)
     (org-with-wide-buffer
      (goto-char org-clock-marker)
      ,@progn)))

(defun org-clock-in-unless-blocked ()
  (interactive)
  (when (org-entry-blocked-p)
    (user-error "Entry is blocked"))
  (org-clock-in))

(defun org-clock-in-next-sibling ()
  (interactive)
  (unless (org-clocking-p)
    (user-error "Not clocking in"))
  (org-clock--with-clock
   (if (org-get-next-sibling)
       (if (org-entry-is-done-p)
           (message "Prevented clocking in, as the entry is done")
         (org-clock-in))
     (message "No next sibling"))))

(defun org-clock-in-parent ()
  (interactive)
  (unless (org-clocking-p)
    (user-error "Not clocking in"))
  (org-clock--with-clock
   (if (org-up-heading-safe)
       (org-clock-in)
     (message "No parent heading"))))

(provide 'init-global-org-map)
