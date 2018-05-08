(use-package lemonbar
  :straight (lemonbar :host github :repo "akirak/lemonbar.el")
  :config
  (lemonbar-start)
  :custom
  (lemonbar-options `("-b"            ; Dock the bar at the bottom of the screen
                      "-g" "1920x20+0+0"
                      "-p"
                      "-f" "Hack-9"))
  (lemonbar-output-template '(akirak/lemonbar-exwm-buffer-list
                              "| "
                              lemonbar-org-clock-string
                              lemonbar-align-right
                              akirak/lemonbar-i3status)))

;;;; Display statistics of X windows
(defvar akirak/lemonbar-exwm-buffer-list nil)

(defun akirak/lemonbar-update-exwm-buffer-list ()
  (let* ((buf-class-list (cl-remove nil
                                    (mapcar (lambda (buf)
                                              (with-current-buffer buf
                                                (when (eq major-mode 'exwm-mode)
                                                  exwm-class-name)))
                                            (buffer-list))))
         (groups (seq-group-by 'identity buf-class-list)))
    (setq akirak/lemonbar-exwm-buffer-list
          (cl-loop for (name . items) in groups
                   concat (concat (format "%%{A:%s:}" (concat "wmctrl -a " name))
                                  name
                                  (when (> (length items) 1)
                                    (format "[%d]" (length items)))
                                  "%{A}"
                                  " ")))
    (lemonbar-update)))

(add-hook 'buffer-list-update-hook #'akirak/lemonbar-update-exwm-buffer-list)
(add-hook 'exwm-manage-finish-hook #'akirak/lemonbar-update-exwm-buffer-list)

;;;; Feeding system information from i3status
(defconst i3status-buffer "*i3status*")

(defcustom i3status-process-filter nil
  "Process filter function for i3status.")

(defun i3status-start ()
  (interactive)
  (let ((proc (start-process "i3status" i3status-buffer
                             "i3status")))
    (set-process-query-on-exit-flag proc nil)
    (when i3status-process-filter
      (set-process-filter proc i3status-process-filter))))

(defun i3status-kill ()
  (interactive)
  (when-let ((proc (get-buffer-process i3status-buffer)))
    (interrupt-process proc)))

;;;;; Integration with lemonbar

(defvar akirak/lemonbar-i3status nil)

(defun akirak/lemonbar-i3status-filter (proc output)
  (setq akirak/lemonbar-i3status (string-trim-right output))
  (lemonbar-update t))
(setq i3status-process-filter 'akirak/lemonbar-i3status-filter)

(add-hook 'lemonbar-start-hook 'i3status-start)
(add-hook 'lemonbar-kill-hook 'i3status-kill)

(with-eval-after-load 'desktop
  (add-hook 'desktop-clear-preserve-buffers "\\*i3status\\*"))

(provide 'init-lemonbar)
