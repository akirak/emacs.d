(use-package Info
  :straight (:type built-in)
  :config

  (cl-defun akirak/info-files (&key full)
    (cl-loop for d in (or Info-directory-list
                          Info-default-directory-list)
             when (file-directory-p d)
             append (directory-files d full "\\.info")))

  (defun akirak/Info-navigate-breadcrumb ()
    (interactive)
    (let* ((pos (text-property-any (point-min) (point-max)
                                   'font-lock-face 'info-header-node))
           (overlay (car-safe (overlays-at pos)))
           (start (and overlay (overlay-start overlay)))
           (end (and overlay (overlay-end overlay)))
           pos
           result)
      (save-excursion
        (goto-char start)
        (while (setq pos (text-property-any (point) end 'font-lock-face 'info-header-xref))
          (push (get-char-property pos 'link-args) result)
          (goto-char (1+ (text-property-not-all pos end 'font-lock-face 'info-header-xref)))))
      (completing-read "Breacrumb: " result)))
  :general
  (:keymaps 'Info-mode-map :package 'info
            "u" #'akirak/info-follow-breadcrumb))

(provide 'setup-info)
