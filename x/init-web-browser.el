;;; init-web-browser.el --- Integration with an external web browser -*- lexical-binding: t -*-

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
                               (equal exwm-class-name "Firefox")))))
                    (window-list)))

(defun akirak/start-browser ()
  (interactive)
  (start-process-shell-command "firefox" nil "firefox"))

(akirak/define-frame-workflow "web"
  :layout
  '(progn
     (akirak/start-browser)
     (delete-other-windows))
  :refocus
  '(unless (akirak/browser-windows)
     (akirak/start-browser))
  :after-kill-buffer
  '(unless (akirak/browser-windows)
     (delete-frame)))

(provide 'init-web-browser)
;;; init-web-browser.el ends here
