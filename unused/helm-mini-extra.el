(defclass helm-exwm-buffer-list-class (helm-source-sync)
  ((candidates :initform
               (lambda ()
                 (when (featurep 'exwm)
                   (cl-loop for (_ . buf) in exwm--id-buffer-alist
                            when (with-current-buffer buf
                                   (eq major-mode 'exwm-mode))
                            collect (cons (with-current-buffer buf exwm-title)
                                          buf)))))))

(defvar helm-source-exwm-buffer-list
  (helm-make-source "EXWM buffers" 'helm-exwm-buffer-list-class
    :action 'exwm-workspace-switch-to-buffer))

(defun akirak/ad-return-helm-buffer-list (bufs)
  (cl-remove-if (lambda (buf)
                  (with-current-buffer buf
                    (eq major-mode 'exwm-mode)))
                bufs))

(advice-add 'helm-buffer-list :filter-return 'akirak/ad-return-helm-buffer-list)

(require 'helm-bookmark)
(setq helm-mini-default-sources `(helm-source-buffers-list
                                  helm-source-exwm-buffer-list
                                  helm-source-buffer-not-found))

(provide 'helm-mini-extra)
