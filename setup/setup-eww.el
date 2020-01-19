(use-package eww
  :straight (:type built-in)
  :config
  (defun eww-tag-pre (dom)
    (let ((shr-folding-mode 'none)
          (shr-current-font 'default))
      (shr-ensure-newline)
      (insert (eww-fontify-pre dom))
      (shr-ensure-newline)))

  (defun eww-fontify-pre (dom)
    (with-temp-buffer
      (shr-generic dom)
      (let ((mode (when (featurep 'language-detection)
                    (eww-buffer-auto-detect-mode))))
        (when mode
          (eww-fontify-buffer mode)))
      (buffer-string)))

  (defun eww-fontify-buffer (mode)
    (delay-mode-hooks (funcall mode))
    (font-lock-default-function mode)
    (font-lock-default-fontify-region (point-min)
                                      (point-max)
                                      nil))

  ;; Requires language-detection package
  (fset 'eww-buffer-auto-detect-mode
        #'akirak/language-detection-buffer-mode)

  (add-to-list 'shr-external-rendering-functions '(pre . eww-tag-pre)))

(provide 'setup-eww)
