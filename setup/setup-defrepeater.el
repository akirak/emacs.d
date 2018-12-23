;;; setup-defrepeater.el

(use-package defrepeater
  :general
  ([remap other-window] (defrepeater #'other-window)
   [remap winner-undo] (defrepeater #'winner-undo)
   [remap winner-redo] (defrepeater #'winner-redo)
   [remap text-scale-increase] (defrepeater #'text-scale-increase)
   [remap text-scale-decrease] (defrepeater #'text-scale-decrease)))

(provide 'setup-defrepeater)
;;; setup-defrepeater.el ends here
