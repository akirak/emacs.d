(use-package perfect-margin
  :straight (perfect-margin :host github :repo "mpwang/perfect-margin")
  :config
  (defun akirak/exwm-window-p (window)
    ;; It seems that a frame is sometimes passed to this function,
    ;; so I have to guard it
    (and (windowp window)
         (eq 'exwm-mode
             (buffer-local-value 'major-mode (window-buffer window)))))
  (add-to-list 'perfect-margin-ignore-filters 'akirak/exwm-window-p)
  (perfect-margin-mode 1)
  :custom
  (perfect-margin-visible-width 92)
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
