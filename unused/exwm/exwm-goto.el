(defun exwm-goto--switch-to-buffer (buf)
  (if-let ((w (get-buffer-window buf t)))
      (select-window w)
    (exwm-workspace-switch-to-buffer buf)))

(cl-defun exwm-goto (command &key class)
  (if-let ((bs (cl-remove-if-not (lambda (buf)
                                   (with-current-buffer buf
                                     (and (eq major-mode 'exwm-mode)
                                          (cond
                                           ((stringp class)
                                            (string-equal class exwm-class-name))))))
                                 (buffer-list))))
      (exwm-goto--switch-to-buffer (car bs))
    (start-process-shell-command class nil command)))

(provide 'exwm-goto)
