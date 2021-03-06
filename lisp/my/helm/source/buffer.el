;; -*- lexical-binding: t -*-

(require 'my/helm/action/buffer)

(defvar akirak/helm-buffer-keymap
  (make-composed-keymap nil akirak/helm-file-like-source-keymap))

(define-key akirak/helm-buffer-keymap
  (kbd "C-c C-k") (lambda ()
                    (interactive)
                    (let ((buffers (or (helm-marked-candidates)
                                       (helm-get-selection))))
                      (mapc #'kill-buffer buffers))))

(defclass akirak/helm-source-buffer (helm-source-sync)
  ((action :initform 'akirak/helm-buffer-actions-1)
   (keymap :initform 'akirak/helm-buffer-keymap)))

;;;; Function for defining sources

(cl-defun akirak/helm-filtered-buffer-source (name predicate &key
                                                   format-candidate
                                                   action)
  (declare (indent 1))
  (-let* (((visible-buffers windows)
           (->> (window-list)
                (-map (lambda (wnd)
                        (let ((buffer (window-buffer wnd)))
                          (when (funcall predicate buffer)
                            (list buffer wnd)))))
                (delq nil)
                (-unzip)
                (-map (lambda (list)
                        (pcase list
                          (`(,x) (list x))
                          (`(,x . ,y) (list x y))
                          (_ list))))))
          (non-visible-buffers (-difference (->> (buffer-list)
                                                 (-filter predicate))
                                            visible-buffers))
          (default-action (lambda (buf)
                            (if current-prefix-arg
                                (progn
                                  (message "Select a window")
                                  (ace-window nil)
                                  (switch-to-buffer buf))
                              (pcase windows
                                (`(,window)
                                 (select-window window)
                                 (switch-to-buffer buf))
                                ('()
                                 (pop-to-buffer buf))
                                (_
                                 (message "Select a window %s" windows)
                                 (ace-window nil)
                                 (switch-to-buffer buf)))))))
    (helm-make-source name 'akirak/helm-source-buffer
      :candidates (-map (lambda (buf)
                          (cons (funcall (or format-candidate #'buffer-name)
                                         buf)
                                buf))
                        non-visible-buffers)
      :action (or action default-action))))

;;;; Concrete sources

(defun akirak/helm-reference-buffer-source ()
  (akirak/helm-filtered-buffer-source "Reference buffers"
    #'akirak/reference-buffer-p))

(defun akirak/helm-exwm-buffer-source ()
  (akirak/helm-filtered-buffer-source "EXWM buffers"
    #'akirak/exwm-buffer-p
    :action akirak/helm-buffer-actions-1))

(defun akirak/helm-scratch-buffer-source ()
  (akirak/helm-filtered-buffer-source "Scratch buffers"
    #'akirak/scratch-buffer-p))

(defun akirak/helm-indirect-org-buffer-source ()
  (akirak/helm-filtered-buffer-source "Indirect Org buffers"
    #'akirak/indirect-org-buffer-p))

(defun akirak/helm-dired-buffer-source ()
  (akirak/helm-filtered-buffer-source "Dired buffers"
    #'akirak/dired-buffer-p
    :format-candidate
    (lambda (buf) (buffer-local-value 'default-directory buf))
    :action akirak/helm-buffer-actions-1))

(defun akirak/helm-dashboard-buffer-source ()
  (akirak/helm-filtered-buffer-source "Dashboard buffers"
    #'akirak/dashboard-buffer-p))

(provide 'my/helm/source/buffer)
