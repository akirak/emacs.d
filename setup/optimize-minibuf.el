;;; optimize-minibuf.el --- Minibuffer optimization -*- lexical-binding: t -*-

;;;; General
(defun optimize-minibuf/not-in-minibuf-p (&rest _args)
  (not (minibuffer-window-active-p (selected-window))))

(advice-add 'run-window-configuration-change-hook
            :before-while
            #'optimize-minibuf/not-in-minibuf-p)

(advice-add 'centaur-tabs-buffer-update-groups
            :before-while
            #'optimize-minibuf/not-in-minibuf-p)

(advice-add 'frame-purpose--buffer-list-update-hook
            :before-while
            #'optimize-minibuf/not-in-minibuf-p)

(advice-add 'feebleline--insert-ignore-errors
            :before-while
            #'optimize-minibuf/not-in-minibuf-p)

;;;; Dimmer

(with-eval-after-load 'dimmer
  (defun optimize-minibuf/dimmer-process-all--fake (&rest _args))

  (defun akirak/dimmer-disable (&rest _args)
    (advice-add 'dimmer-process-all :override
                'optimize-minibuf/dimmer-process-all--fake))

  (defun akirak/dimmer-enable (&rest _args)
    (advice-remove 'dimmer-process-all 'optimize-minibuf/dimmer-process-all--fake))

  (add-hook 'minibuffer-setup-hook 'akirak/dimmer-disable)
  (add-hook 'minibuffer-exit-hook 'akirak/dimmer-enable)

  (advice-add 'read-key
              :around
              (defun akirak/ad-around-read-key (orig &rest args)
                (ignore-errors
                  (akirak/dimmer-disable))
                (unwind-protect
                    (apply orig args)
                  (akirak/dimmer-enable))))

  (advice-add 'org-starter--display-options
              :before #'akirak/dimmer-disable)
  (advice-add 'org-starter--delete-message-frame
              :after #'akirak/dimmer-enable))

;;;; Helm
(with-eval-after-load 'helm
  (defsubst optimize-minibuf/not-helm-major-mode-p (&rest _args)
    (eq major-mode 'helm-major-mode))

  (advice-add 'yankpad-local-category-to-major-mode
              :before-while #'optimize-minibuf/not-helm-major-mode-p))

(provide 'optimize-minibuf)
;;; optimize-minibuf.el ends here
