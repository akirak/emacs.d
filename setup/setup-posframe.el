(use-package posframe
  :config
  (defun akirak/frame-contains-exwm-window-p (&optional frame)
    (--any (eq 'exwm-mode (buffer-local-value 'major-mode (window-buffer it)))
           (window-list frame)))
  (defun akirak/posframe-poshandler-smart-center (info)
    (if (akirak/frame-contains-exwm-window-p)
        (posframe-poshandler-window-center info)
      (posframe-poshandler-frame-center info))))

(provide 'setup-posframe)
