;; -*- lexical-binding: t -*-

(require 'my/helm/action/buffer)

(defclass akirak/helm-source-buffer (helm-source-sync)
  ((action :initform 'akirak/helm-buffer-actions-1)))

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
    (helm-make-source name 'helm-source-sync
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

(provide 'my/helm/source/buffer)
