;;;; Visualise whitespace

(defun akirak/show-trailing-whitespace ()
  (setq-local prog-mode show-trailing-whitespace t))

(general-add-hook '(prog-mode text-mode sgml-mode)
                  #'akirak/show-trailing-whitespace)

(use-package whitespace
  :disabled t
  :straight nil
  :diminish whitespace-mode
  :hook
  ((prog-mode sgml-mode) . whitespace-mode)
  :custom
  (whitespace-style '(trailing)))

;;;; Automatically cleaning up whitespace
(use-package whitespace-cleanup-mode
  :commands (whitespace-cleanup-mode)
  :hook
  ;; Turn on whitespace-cleanup-mode if and only if you need it
  ;;
  ;; In `makefile-mode', whitespace-cleanup-mode automatically
  ;; converts 8 spaces into a tab, because `indentation' is included
  ;; in `whitespace-style' and `indent-tabs-mode' is on.
  ((makefile-mode) .
   whitespace-cleanup-mode))

;;;; Manually shrink whitespace

(defun akirak/shrink-whitespace ()
  "An alternative to `delete-horizontal-space'."
  (interactive)
  (cond
   ((and (integerp current-prefix-arg)
         (>= current-prefix-arg 0))
    ;; With an argument of non-negative integer, set the number of
    ;; whitespaces around the cursor to the argument.
    (if (looking-at (rx (* space) eol))
        (progn
          (end-of-line)
          (insert (make-string (max 0 (- current-prefix-arg
                                         (car (posn-col-row (posn-at-point)))))
                               32)))
      (delete-horizontal-space)
      (insert (make-string current-prefix-arg 32))))
   ((eql current-prefix-arg '(4))
    (delete-blank-lines))
   ((and (not current-prefix-arg)
         (or (looking-at (rx (* space) eol))
             (looking-back (rx bol (+ space)))))
    ;; Without a prefix, if the cursor is either at the beginning or
    ;; the end of the line, delete all the whites0aces.
    (delete-horizontal-space))
   (t (call-interactively 'cycle-spacing))))
(general-def [remap delete-horizontal-space] 'akirak/shrink-whitespace)

(provide 'setup-whitespace)
