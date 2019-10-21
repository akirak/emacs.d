;; https://stackoverflow.com/questions/18316665/how-to-improve-emacs-performance-when-view-large-file
(defun my-find-file-check-make-large-file-read-only-hook ()
  "If a file is over a given size, make the buffer read only."
  (when (> (buffer-size) (* 512 1024))
    (setq buffer-read-only t)
    (buffer-disable-undo)
    (unless (derived-mode-p 'pdf-view-mode 'nov-mode)
      (fundamental-mode))))

(add-hook 'find-file-hook 'my-find-file-check-make-large-file-read-only-hook)

(provide 'setup-large-files)
