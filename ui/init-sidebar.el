(use-package ibuffer-sidebar
  :config
  ;; Add support for per-frame ibuffer-sidebar buffers.
  ;; This is important if you use frame-purpose.
  (defun akirak/ibuffer-sidebar-get-or-create-buffer ()
    "Get or create a `ibuffer-sidebar' buffer."
    (or (ibuffer-sidebar-buffer)
        (let ((new-buffer (generate-new-buffer ibuffer-sidebar-name)))
          (with-current-buffer new-buffer
            (ibuffer-sidebar-setup))
          new-buffer)))
  (advice-add #'ibuffer-sidebar-get-or-create-buffer
              :override #'akirak/ibuffer-sidebar-get-or-create-buffer)
  ;; kill-this-buffer
  (dolist (command '(kill-this-buffer
                     multi-term
                     eshell))
    (add-to-list 'ibuffer-sidebar-special-refresh-commands command)))

(use-package dired-sidebar)

(provide 'init-sidebar)
