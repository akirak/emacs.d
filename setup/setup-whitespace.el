;;;; shrink-whitepsace commmand

(defun akirak/shrink-whitespace ()
  "An alternative to `delete-horizontal-space'."
  (interactive)
  (cond
   ((and (integerp current-prefix-arg)
         (>= current-prefix-arg 0))
    (if (looking-at (rx (* space) eol))
        (progn
          (end-of-line)
          (insert (make-string (max 0 (- current-prefix-arg
                                         (car (posn-col-row (posn-at-point)))))
                               32)))
      (delete-horizontal-space)
      (insert (make-string current-prefix-arg 32))))
   ((and (not current-prefix-arg)
         (looking-at (rx (* space) eol)))
    (delete-horizontal-space))                
   (t (call-interactively 'cycle-spacing))))
(general-def [remap delete-horizontal-space] 'akirak/shrink-whitespace)

(provide 'setup-whitespace)
