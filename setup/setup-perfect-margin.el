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

(el-patch-defun window-splittable-p (window &optional horizontal)
  "Return non-nil if `split-window-sensibly' may split WINDOW.
Optional argument HORIZONTAL nil or omitted means check whether
`split-window-sensibly' may split WINDOW vertically.  HORIZONTAL
non-nil means check whether WINDOW may be split horizontally.

WINDOW may be split vertically when the following conditions
hold:
- `window-size-fixed' is either nil or equals `width' for the
  buffer of WINDOW.
- `split-height-threshold' is an integer and WINDOW is at least as
  high as `split-height-threshold'.
- When WINDOW is split evenly, the emanating windows are at least
  `window-min-height' lines tall and can accommodate at least one
  line plus - if WINDOW has one - a mode line.

WINDOW may be split horizontally when the following conditions
hold:
- `window-size-fixed' is either nil or equals `height' for the
  buffer of WINDOW.
- `split-width-threshold' is an integer and WINDOW is at least as
  wide as `split-width-threshold'.
- When WINDOW is split evenly, the emanating windows are at least
  `window-min-width' or two (whichever is larger) columns wide."
  (when (and (window-live-p window)
             (not (window-parameter window 'window-side)))
    (with-current-buffer (window-buffer window)
      (if horizontal
          ;; A window can be split horizontally when its width is not
          ;; fixed, it is at least `split-width-threshold' columns wide
          ;; and at least twice as wide as `window-min-width' and 2 (the
          ;; latter value is hardcoded).
          (and (memq window-size-fixed '(nil height))
               ;; Testing `window-full-width-p' here hardly makes any
               ;; sense nowadays.  This can be done more intuitively by
               ;; setting up `split-width-threshold' appropriately.
               (numberp split-width-threshold)
               (>= (el-patch-swap (window-width window)
                                  (if (bound-and-true-p perfect-margin-mode)
                                      (perfect-margin--width-with-margins window)
                                    (window-width window)))
                   (max split-width-threshold
                        (* 2 (max window-min-width 2)))))
        ;; A window can be split vertically when its height is not
        ;; fixed, it is at least `split-height-threshold' lines high,
        ;; and it is at least twice as high as `window-min-height' and 2
        ;; if it has a mode line or 1.
        (and (memq window-size-fixed '(nil width))
             (numberp split-height-threshold)
             (>= (window-height window)
                 (max split-height-threshold
                      (* 2 (max window-min-height
                                (if mode-line-format 2 1))))))))))

(provide 'setup-perfect-margin)
