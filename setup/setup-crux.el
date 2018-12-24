(use-package crux
  :config
  ;; Originally from https://github.com/bbatsov/crux/blob/master/crux.el
  ;;
  ;; Changes:
  ;;
  ;; - Use =read-file-name= to ask for a file name
  ;; - Apply =akirak/update-buffer-after-renaming= after renaming
  (defun crux-rename-file-and-buffer ()
    "Rename current buffer and if the buffer is visiting a file, rename it too."
    (interactive)
    (let ((filename (buffer-file-name)))
      (if (not (and filename (file-exists-p filename)))
          (rename-buffer (read-from-minibuffer "New name: " (buffer-name)))
        (let* ((new-name (read-file-name (format "New file name [from %s]: "
                                                 (abbreviate-file-name filename))
                                         nil filename))
               (containing-dir (file-name-directory new-name)))
          (make-directory containing-dir t)
          (cond
           ((vc-backend filename) (vc-rename-file filename new-name))
           (t
            (rename-file filename new-name t)
            (set-visited-file-name new-name t t)
            ;; See setup-rename.el
            (when (fboundp 'akirak/update-buffer-after-renaming)
              (akirak/update-buffer-after-renaming filename)))))))))

(provide 'setup-crux)
;;; setup-crux.el ends here
