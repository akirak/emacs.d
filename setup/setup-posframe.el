(use-package posframe
  :config
  (defun akirak/frame-contains-exwm-window-p (&optional frame)
    (--any (eq 'exwm-mode (buffer-local-value 'major-mode (window-buffer it)))
           (window-list frame)))
  (defun akirak/posframe-poshandler-smart-center (info)
    (cond
     ((< (window-width) (or (plist-get info :posframe-width)
                           90))
      (posframe-poshandler-window-bottom-left-corner info))
    ((akirak/frame-contains-exwm-window-p)
     (posframe-poshandler-window-center info))
    (t
     (posframe-poshandler-frame-center info)))))

(provide 'setup-posframe)
