(use-package super-save
  ;; :diminish 'super-save-mode
  :config
  (setq akirak/super-save-prevented-modes
        '(makefile-mode))
  (super-save-mode 1)
  :config/el-patch
  ;; (el-patch-feature super-save)
  (el-patch-defun super-save-command ()
    "Save the current buffer if needed."
    (when (and buffer-file-name
               (buffer-modified-p (current-buffer))
               (file-writable-p buffer-file-name)
               (el-patch-add (apply #'derived-mode-p akirak/super-save-prevented-modes))
               (if (file-remote-p buffer-file-name) super-save-remote-files t))
      (save-buffer)))
  :custom
  (super-save-auto-save-when-idle t))

(provide 'setup-super-save)
