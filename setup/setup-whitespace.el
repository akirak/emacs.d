;;;; Visualise whitespace
(use-package whitespace
  :straight nil
  :diminish whitespace-mode
  :config
  (global-whitespace-mode t)
  :custom
  (whitespace-style '(face tabs indentation trailing tab-mark empty)))

;;;; Automatically cleaning up whitespace
(use-package whitespace-cleanup-mode
  :custom
  (whitespace-cleanup-mode-enabled nil)
  :config
  (global-whitespace-mode t)
  :hook
  ;; Turn on whitespace-cleanup-mode if and only if you need it
  ;;
  ;; In `makefile-mode', whitespace-cleanup-mode automatically
  ;; converts 8 spaces into a tab, because `indentation' is included
  ;; in `whitespace-style' and `indent-tabs-mode' is on.
  ((makefile-mode) .
   whitespace-cleanup-mode))

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
         (or (looking-at (rx (* space) eol))
             (looking-back (rx bol (+ space)))))
    (delete-horizontal-space))                
   (t (call-interactively 'cycle-spacing))))
(general-def [remap delete-horizontal-space] 'akirak/shrink-whitespace)

(provide 'setup-whitespace)
