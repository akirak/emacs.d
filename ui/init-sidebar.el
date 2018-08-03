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

(use-package dired-sidebar
  :config
  (defun akirak/dired-sidebar-find-file ()
    (interactive)
    (let ((file (dired-get-file-for-visit))
          (window (selected-window)))
      ;; This may not be enough
      (setq window (window-in-direction 'right window))
      (select-window window)
      (find-file file)))
  (defun akirak/dired-sidebar-preview-file ()
    (interactive)
    (let ((orig-window (selected-window)))
      (akirak/dired-sidebar-find-file)
      (select-window orig-window)))
  :general
  (:keymaps 'dired-sidebar-mode-map :package 'dired-sidebar
            "SPC" #'akirak/dired-sidebar-preview-file
            "RET" #'akirak/dired-sidebar-find-file))

(provide 'init-sidebar)
