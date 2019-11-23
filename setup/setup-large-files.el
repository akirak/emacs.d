(setq-default large-file-warning-threshold (* 512 1024))

;; https://stackoverflow.com/questions/18316665/how-to-improve-emacs-performance-when-view-large-file
(defun my-find-file-check-make-large-file-read-only-hook ()
  "If a file is over a given size, make the buffer read only."
  (when (> (buffer-size) (* 512 1024))
    (setq buffer-read-only t)
    (buffer-disable-undo)
    (unless (derived-mode-p 'pdf-view-mode 'nov-mode)
      (fundamental-mode))))

(add-hook 'find-file-hook 'my-find-file-check-make-large-file-read-only-hook)

(defun akirak/ad-before-while-abort-if-file-too-large (size op-type filename)
  (not (member (file-name-nondirectory filename) '("TAGS"))))

(advice-add 'abort-if-file-too-large :before-while
            #'akirak/ad-before-while-abort-if-file-too-large)

(provide 'setup-large-files)
