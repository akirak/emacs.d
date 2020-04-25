(defun akirak/org-subtree-temporary-posframe (marker bname)
  (when (and (get-buffer bname)
             (buffer-live-p (get-buffer bname)))
    (posframe-delete bname))
  (let* ((orig-buffer (marker-buffer marker))
         (buffer (with-current-buffer (or (org-base-buffer orig-buffer)
                                          orig-buffer)
                   (goto-char marker)
                   (widen)
                   (let ((pos (point))
                         (buffer (org-get-indirect-buffer)))
                     (with-current-buffer buffer
                       (goto-char pos)
                       (setq org-clock-childframe--buffer (point-marker))
                       (org-narrow-to-subtree)
                       (rename-buffer bname)
                       (org-show-entry)
                       (setq-local cursor-type t)
                       (setq-local header-line-format
                                   (list (buffer-name orig-buffer))))
                     buffer))))
    buffer))

(provide 'my/org/posframe)