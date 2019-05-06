(use-package perfect-margin
  :straight (perfect-margin :host github :repo "mpwang/perfect-margin")
  :config
  (perfect-margin-mode 1)
  :custom
  (perfect-margin-visible-width 92)
  (perfect-margin-ignore-modes '(exwm-mode
                                 doc-view-mode
                                 nov-mode))
  (perfect-margin-ignore-regexps `("^minibuf" "^[*]"
                                   "^ \\*LV\\*"
                                   "^ \\*which-key\\*")))

(defun akirak/perfect-margin-window-splittable-p (window &optional horizontal)
  (when (and (window-live-p window)
             (not (window-parameter window 'window-side)))
    (with-current-buffer (window-buffer window)
      (if horizontal
	  (and (memq window-size-fixed '(nil height))
	       (numberp split-width-threshold)
               ;; This part has been changed from the original version.
	       (>= (if (bound-and-true-p perfect-margin-mode)
                       (perfect-margin--width-with-margins window)
                     (window-width window))
		   (max split-width-threshold
			(* 2 (max window-min-width 2)))))
	(and (memq window-size-fixed '(nil width))
	     (numberp split-height-threshold)
	     (>= (window-height window)
		 (max split-height-threshold
		      (* 2 (max window-min-height
				(if mode-line-format 2 1))))))))))

(advice-add #'window-splittable-p :override
            #'akirak/perfect-margin-window-splittable-p)

(provide 'setup-perfect-margin)
