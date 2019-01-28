(use-package expand-region)

(defun akirak/kill-word ()
  (interactive)
  (er/mark-word)
  (call-interactively #'kill-region)
  (just-one-space))

(defun akirak/kill-sentence ()
  (interactive)
  (er/mark-sentence)
  (call-interactively #'kill-region)
  (just-one-space))

(defun akirak/kill-defun ()
  (interactive)
  (er/mark-defun)
  (call-interactively #'kill-region)
  (just-one-space))

(provide 'init-expand-region)
