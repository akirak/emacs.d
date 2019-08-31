(use-package posframe
  :config
  (defun akirak/frame-contains-exwm-window-p (&optional frame)
    (--any (eq 'exwm-mode (buffer-local-value 'major-mode (window-buffer it)))
           (window-list frame)))
  (defun akirak/posframe-poshandler-smart-center (info)
    (let ((posframe-width (if-let ((pixel-width (plist-get info :posframe-width)))
                              (/ pixel-width (frame-char-width))
                            90))
          (window-width (window-width))
          (frame-width (frame-width))
          (has-exwm-window (akirak/frame-contains-exwm-window-p)))
      (cond
       ((and has-exwm-window (< window-width posframe-width))
        (posframe-poshandler-window-bottom-left-corner info))
       (has-exwm-window
        (posframe-poshandler-window-center info))
       ((< frame-width posframe-width)
        (posframe-poshandler-frame-bottom-left-corner info))
       (t
        (posframe-poshandler-frame-center info))))))

(provide 'setup-posframe)
