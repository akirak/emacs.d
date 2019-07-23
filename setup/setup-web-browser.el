;;; setup-web-browser.el --- Integration with external web browsers -*- lexical-binding: t -*-

;; In EXWM, prefer Chromium installed on the guest operating system
;; over Chrome/Chromium installed on Chrome/Chromium OS.
(when akirak/to-be-run-as-exwm
  (setq-default browse-url-browser-function browse-url-generic
                browse-url-generic-program "chromium"))

(defcustom akirak/browser-class-names '("Chromium"
                                        "Firefox")
  "List of X class names of web browsers."
  :type '(repeat string))

(defun akirak/display-url-for-referencing (url)
  (interactive "sUrl: ")
  (let ((orig-win (selected-window)))
    (split-window-sensibly)
    (other-window 1)
    (eww url)
    (select-window orig-win)))

(defun akirak/browser-windows ()
  "Return a list of web browser windows in the frame."
  (cl-remove-if-not (lambda (window)
                      (let ((buf (window-buffer window)))
                        (with-current-buffer buf
                          (and (eq major-mode 'exwm-mode)
                               (member exwm-class-name akirak/browser-class-names)))))
                    (window-list)))

(defun akirak/start-browser ()
  (interactive)
  (start-process-shell-command "chromium" nil "chromium"))

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
