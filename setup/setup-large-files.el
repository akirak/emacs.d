(use-package files
  :straight (:type built-in)
  ;; This does not work in Emacs 27. I have to investigate the issue.
  ;;
  ;; :config/el-patch
  ;; (el-patch-defun abort-if-file-too-large (size op-type filename)
  ;;   (when (and (el-patch-add (not (string-match-p (rx (or ".el" ".el.gz") eol) filename)))
  ;;              (el-patch-add (not (member (file-name-nondirectory filename)
  ;;                                         '("TAGS"))))
  ;;              large-file-warning-threshold size
  ;;              (> size large-file-warning-threshold)
  ;;              (not (y-or-n-p (format "File %s is large (%s), really %s? "
  ;;                                     (file-name-nondirectory filename)
  ;;                                     (file-size-human-readable size) op-type))))
  ;;     (user-error "Aborted")))
  :config
  ;; https://stackoverflow.com/questions/18316665/how-to-improve-emacs-performance-when-view-large-file
  (defun my-find-file-check-make-large-file-read-only-hook ()
    "If a file is over a given size, make the buffer read only."
    (when (> (buffer-size) large-file-warning-threshold)
      (setq buffer-read-only t)
      (buffer-disable-undo)
      (unless (derived-mode-p 'pdf-view-mode 'nov-mode
                              'emacs-lisp-mode)
        (fundamental-mode))))
  :hook
  (find-file . my-find-file-check-make-large-file-read-only-hook)
  :custom
  (large-file-warning-threshold (* 1024 1024)))

(provide 'setup-large-files)
