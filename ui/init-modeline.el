(use-package relative-buffers
  :config
  (global-relative-buffers-mode))

;; Display both line and column numbers in the modeline
(setq column-number-mode 1
      line-number-mode 1)

(setq mode-line-position
      '(buffer-read-only
        ("%p")
        ("("
         (line-number-mode
          ("%l" (column-number-mode ",%c")))
         ")")))

;;;;; EXWM
(defun akirak//mode-line-exwm-workspace (fmt &optional default)
  (if-let ((index (and (fboundp 'exwm-workspace--position)
                       (exwm-workspace--position (selected-frame)))))
      (format fmt index)
    default))

(setq akirak/mode-line-exwm-workspace
      '(akirak//mode-line-exwm-frame-workspace "X:%d "))

(setq-default mode-line-format
              '((:eval (when (fboundp 'frame-workflow-mode-line)
                         (frame-workflow-mode-line)))
                "%e"
                mode-line-front-space
                mode-line-mule-info
                mode-line-client
                mode-line-modified
                mode-line-remote
                mode-line-frame-identification
                mode-line-buffer-identification
                "   "
                mode-line-position
                (vc-mode vc-mode)
                "  "
                mode-line-modes
                mode-line-misc-info
                mode-line-end-spaces))

(provide 'init-modeline)
