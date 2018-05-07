(defun akirak/set-frame-font ()
  (interactive)
  (set-frame-font "Hack-10.5"))

(defun akirak/set-all-frame-font ()
  (interactive)
  (dolist (frame (frame-list))
    (with-selected-frame frame
      (akirak/set-frame-font))))

;; TODO: There may be a better way to do this
(akirak/set-frame-font)

(provide 'init-font)
