(use-package desktop
  :init
  (cl-defun akirak/add-desktop-settings (&key clear-preserve-buffers
                                              globals-to-save
                                              globals-to-clear
                                              locals-to-save)
    "Add items to variables for `desktop-mode'."
    (general-add-hook 'desktop-clear-preserve-buffers
                      clear-preserve-buffers)
    (general-add-hook 'desktop-globals-to-save
                      globals-to-save)
    (general-add-hook 'desktop-globals-to-save
                      globals-to-clear)
    (general-add-hook 'desktop-locals-to-save
                      locals-to-save))
  :config
  ;; Save the desktop when Emacs exits or switch to another desktop
  ;; directory.
  (unless (daemonp)
    (desktop-save-mode t))
  :custom
  (desktop-save 'ask-if-exists)
  ;; (desktop-restore-in-current-display)
  (desktop-restore-reuses-frames 'keep))

(provide 'setup-desktop)
