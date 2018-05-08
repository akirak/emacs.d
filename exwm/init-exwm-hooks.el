;;;; Set the buffer name to the window title
(add-hook 'exwm-update-title-hook
          (lambda () (exwm-workspace-rename-buffer exwm-title)))

;;;; Enter char-mode automatically
(defcustom akirak/exwm-char-mode-window-classes
  '("Chromium" "Termite" "Emacs" "Firefox")
  "List of instance names of windows that should start in char-mode.")

(defun akirak/exwm-set-char-mode-on-specific-windows ()
  (when (member exwm-class-name akirak/exwm-char-mode-window-classes)
    (exwm-input-release-keyboard (exwm--buffer->id (window-buffer)))))

(add-hook 'exwm-manage-finish-hook 'akirak/exwm-set-char-mode-on-specific-windows)

;;;; Local prefix keys
;; You can set local prefix keys, but I won't use this features for now.
;; I prefer char-mode. 
(defun akirak/exwm-set-local-input-prefix-keys ()
  (when (or (string-equal exwm-instance-name "chromium")
            (string-equal exwm-class-name "Firefox"))
    (setq-local exwm-input-prefix-keys nil)))
;; (add-hook 'exwm-manage-finish-hook 'akirak/exwm-set-local-input-prefix-keys)

;;;; Automatically float windows
(defcustom akirak/exwm-floating-window-classes '("keybase")
  "List of instance names of windows that should start in the floating mode.")

(defun akirak/exwm-float-window-on-specific-windows ()
  (when (member exwm-instance-name akirak/exwm-floating-window-classes)
    (exwm-floating-toggle-floating)))
(add-hook 'exwm-manage-finish-hook #'akirak/exwm-float-window-on-specific-windows)

(provide 'init-exwm-hooks)
