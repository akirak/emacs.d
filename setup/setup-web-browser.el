;;; setup-web-browser.el --- Integration with external web browsers -*- lexical-binding: t -*-

;; In EXWM, prefer Chromium installed on the guest operating system
;; over Chrome/Chromium installed on Chrome/Chromium OS.
(when akirak/to-be-run-as-exwm
  (setq-default browse-url-browser-function 'browse-url-generic
                browse-url-generic-program "chromium"))

(defun akirak/display-url-for-referencing (url)
  (interactive "sUrl: ")
  (let ((orig-win (selected-window)))
    (split-window-sensibly)
    (other-window 1)
    (eww url)
    (select-window orig-win)))

(defcustom akirak/browser-class-names '("Chromium"
                                        "Chromium-browser"
                                        "Firefox")
  "List of X class names of web browsers."
  :type '(repeat string))

(defun akirak/exwm-browser-buffer-p (buffer)
  ;; (and (eq 'exwm-mode (buffer-local-value 'major-mode buffer))
  ;;      (member (buffer-local-value 'exwm-class-name buffer)
  ;;              akirak/browser-class-names))
  (when (stringp buffer)
    (setq buffer (get-buffer buffer)))
  (unless buffer
    (user-error "BUFFER cannot be nil"))
  (member (buffer-local-value 'exwm-class-name buffer)
          akirak/browser-class-names))

(defun akirak/exwm-browser-windows ()
  (-filter (lambda (w)
             (akirak/exwm-browser-buffer-p (window-buffer w)))
           (window-list)))

(defun akirak/select-exwm-browser-window ()
  (when-let ((browser-windows (akirak/exwm-browser-windows)))
    (if (= 1 (length browser-windows))
        (car browser-windows)
      (get-buffer-window (get-buffer
                          (completing-read "Browsers"
                                           (mapcar (lambda (w)
                                                     (buffer-name (window-buffer w)))
                                                   browser-windows)))))))

(defun akirak/exwm-browser-buffers ()
  (-filter #'akirak/exwm-browser-buffer-p (akirak/exwm-list-buffers)))

(defun akirak/select-exwm-browser-buffer ()
  (when-let ((buffers (akirak/exwm-browser-buffers)))
    (if (= 1 (length buffers))
        (get-buffer (car buffers))
      (get-buffer (completing-read "Browsers"
                                   (mapcar #'buffer-name buffers))))))

(defun akirak/start-web-browser ()
  (interactive)
  (start-process-shell-command "browser" nil
                               (or browse-url-generic-program
                                   "chromium")))

(defun akirak/raise-browser (&optional arg)
  (interactive "P")
  (cond
   ((featurep 'exwm)
    (cl-case arg
      ('(4)
       (akirak/start-web-browser))
      (otherwise
       (if-let ((w (akirak/select-exwm-browser-window)))
           (select-window w)
         (if-let ((b (akirak/select-exwm-browser-buffer)))
             (switch-to-buffer-other-window b)
           (akirak/start-web-browser))))))))

(akirak/define-frame-workflow "web"
  :key "w"
  :layout
  ;; Start a browser and delete the other windows
  '(progn
     (akirak/start-browser)
     (delete-other-windows))
  :refocus
  ;; If there is no browser window in the frame, open a new
  ;; browser instance.
  '(unless (akirak/browser-windows)
     (akirak/start-browser))
  :after-kill-buffer
  ;; Delete the frame.
  ;; If there is a browser window in the frame, don't delete the frame.
  ;; If the frame is the only frame, don't delete the frame.
  '(unless (or (akirak/browser-windows)
               (= 1 (length (frame-list))))
     (delete-frame)))

(provide 'setup-web-browser)
;;; setup-web-browser.el ends here
