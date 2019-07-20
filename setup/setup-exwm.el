(use-package exwm
  :commands (exwm-enable)
  :custom
  (exwm-floating-border-width 3)
  (exwm-floating-border-color "orange")
  :hook
  (exwm-update-title . akirak/exwm-rename-buffer)
  (exwm-manage-finish . akirak/exwm-manage-finish))

(defun akirak/exwm-rename-buffer ()
  "Rename the buffer name after the title is changed."
  (exwm-workspace-rename-buffer exwm-title))

(defun akirak/exwm-manage-finish ()
  (cond
   ;; Set char mode on specific window types
   ((member exwm-class-name '("Chromium" "Termite" "Emacs" "Firefox"))
    (exwm-input-release-keyboard (exwm--buffer->id (window-buffer))))
   ;; Float specific window types
   ((member exwm-instance-name '("keybase"))
    (exwm-floating-toggle-floating))))

(use-package exwm-config
  :straight exwm)

(setq mouse-autoselect-window t)

;; Mouse follows focus
(use-package exwm-mff
  :straight (exwm-mff :host github :repo "ieure/exwm-mff"))

(use-package exwm-edit
  :config
  (defun akirak/exwm-edit-setup-compose ()
    (let ((title (exwm-edit--buffer-title (buffer-name))))
      (cond
       ;; Add customization
       )))
  :hook
  (exwm-edit-compose . akirak/exwm-edit-setup-compose))

(use-package window-divider
  :disabled t
  :config
  (window-divider-mode 1)
  :custom
  (window-divider-default-right-width 2)
  (window-divider-default-bottom-width 2))

(use-package exwm-systemtray
  :straight exwm
  :config
  (exwm-systemtray-enable)
  :custom
  (exwm-systemtray-height 16))

(provide 'setup-exwm)
