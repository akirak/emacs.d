(use-package lemonbar
  :straight (lemonbar :host github :repo "akirak/lemonbar.el")
  :config
  (lemonbar-set-output-template '(lemonbar-org-clock-color
                                  "  "
                                  akirak/lemonbar-exwm-workspace-list
                                  "  "
                                  akirak/lemonbar-exwm-buffer-list
                                  lemonbar-align-center
                                  lemonbar-org-clock-string
                                  lemonbar-align-right
                                  ;; Somehow this doesn't work now
                                  ;; akirak/lemonbar-i3status
                                  (:eval (format-time-string "%F (%a) %R %Z"))
                                  "  "
                                  lemonbar-org-clock-reset-color))
  (lemonbar-start)
  :custom
  (lemonbar-options '("-b"            ; Dock the bar at the bottom of the screen
                      "-g" "1920x22+0+0"
                      "-p"
                      "-f" "Noto Sans-10.5:medium:italic")))

;;;; EXWM workspaces
(defvar akirak/lemonbar-exwm-workspace-list nil)

(defun akirak/lemonbar-update-exwm-workspace-list ()
  (setq akirak/lemonbar-exwm-workspace-list
        (mapconcat (lambda (i)
                     (let* ((frm (exwm-workspace--workspace-from-frame-or-index i))
                            (name (frame-parameter frm 'workflow-prototype)))
                       (format
                        (cond
                         ((equal frm (selected-frame)) "[%s*]")
                         ;; TODO: A better way to detect active workspaces
                         ;; This does not always detect all active workspaces.
                         ((exwm-workspace--active-p frm) "[%s]")
                         (t "%s"))
                        (concat (int-to-string i)
                                (if name
                                    (concat ":" (symbol-name name))
                                  "")))))
                   (number-sequence 0 (1- (exwm-workspace--count)))
                   " "))
  (lemonbar-update))

(add-hook 'exwm-workspace-list-change-hook #'akirak/lemonbar-update-exwm-workspace-list)
(add-hook 'exwm-workspace-switch-hook #'akirak/lemonbar-update-exwm-workspace-list)
(add-hook 'frame-workflow-set-prototype-hook #'akirak/lemonbar-update-exwm-workspace-list)

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

;;;; Integration with org-clock
(require 'lemonbar-org-clock)

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
