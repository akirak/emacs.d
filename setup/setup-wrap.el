;;; Word wrap and visual-line-mode
(general-add-hook '(org-mode-hook
                    markdown-mode-hook)
                  '(toggle-word-wrap
                    (lambda () (visual-line-mode 1))))

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
