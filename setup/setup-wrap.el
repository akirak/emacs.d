;;; Word wrap and visual-line-mode
(use-package adaptive-wrap
  :commands (adaptive-wrap-prefix-mode)
  :config
  (defun akirak/adaptive-wrap-prefix-mode ()
    (unless (derived-mode-p 'org-mode)
      (adaptive-wrap-prefix-mode)))
  :hook
  (visual-line-mode . akirak/adaptive-wrap-prefix-mode))

(global-visual-line-mode t)

;; Suppress a message by `toggle-word-wrap' unless the function is
;; called interactively. The message is annoying as the option is set
;; on Org files by default.
(defun akirak/ad-around-toggle-word-wrap (orig &optional arg)
  (unwind-protect
      (progn
        (unless (called-interactively-p)
          (advice-add 'message :override #'akirak/ignore-toggle-word-wrap-message))
        (funcall orig arg))
    (advice-remove 'message #'akirak/ignore-toggle-word-wrap-message)))
(advice-add #'toggle-word-wrap :around #'akirak/ad-around-toggle-word-wrap)

(defun akirak/ignore-toggle-word-wrap-message (&rest args)
  (apply 'ignore args))

(provide 'setup-wrap)
