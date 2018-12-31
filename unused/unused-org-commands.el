(defconst akirak/org-clock-buffer-name "*org clocking*"
  "Name for the indirect buffer for the clocking task.")

(defun akirak/pop-up-org-clocking-task (arg)
  "Open an indirect buffer for the currently clocked task in a window.

When a prefix ARG is given, display the buffer in a window but don't
select it."
  (interactive "P")
  (unless (org-clocking-p)
    (user-error "There is no currently running clock"))
  (let* ((bufname akirak/org-clock-buffer-name)
         (buf (get-buffer bufname)))
    (when (and buf
               (buffer-live-p buf))
      ;; Compare the IDs of the two entries
      (unless (equal (with-current-buffer (org-clocking-buffer)
                       (org-entry-get org-clock-marker "ID"))
                     (with-current-buffer buf
                       (org-entry-get (point-min) "ID")))
        (kill-buffer buf)
        (setq buf nil)))
    (unless buf
      (setq buf
            (let ((org-indirect-buffer-display 'current-window)
                  (wconf (current-window-configuration)))
              (unwind-protect
                  (progn
                    (switch-to-buffer (org-clocking-buffer))
                    (org-with-wide-buffer
                     (org-goto-marker-or-bmk org-clock-marker)
                     (org-tree-to-indirect-buffer)
                     (rename-buffer akirak/org-clock-buffer-name)
                     (current-buffer)))
                (set-window-configuration wconf)))))
    (if-let ((window (get-buffer-window buf)))
        (unless arg
          (select-window window))
      (if arg
          (display-buffer buf)
        (pop-to-buffer buf)))))

(provide 'unused-org-commands)
