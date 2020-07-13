(use-package ace-window
  :config
  (defun akirak/aw-quit-window (window)
    "Delete window WINDOW."
    (let ((frame (window-frame window)))
      (when (and (frame-live-p frame)
                 (not (eq frame (selected-frame))))
        (select-frame-set-input-focus (window-frame window)))
      (if (= 1 (length (window-list)))
          (progn
            (bury-buffer (window-buffer window))
            (delete-frame frame))
        (if (window-live-p window)
            (quit-window window)
          (error "Got a dead window %S" window)))))
  (custom-theme-set-faces 'user
                          '(aw-leading-char-face
                            ((default
                               :background "gray18" :foreground "tan"
                               :height 250))))
  (advice-add 'aw-delete-window
              :after
              (defun akirak/ad-after-aw-delete-window (&rest _args)
                (balance-windows)))
  (advice-add 'aw-delete-window
              :around
              (defun akirak/ad-around-aw-delete-window (origfun &rest args)
                (let ((initial-window (selected-window)))
                  (prog1 (apply origfun args)
                    (when (window-live-p initial-window)
                      (select-window initial-window))))))
  (setq aw-scope (cond
                  (akirak/to-be-run-as-exwm 'visible)
                  (t 'frame)))
  :custom
  (aw-keys (string-to-list "qwertyui"))
  (aw-background nil)
  ;; Prevent an error from aw-set-make-frame-char
  (aw-make-frame-char nil)
  ;; Ergonomic bindings
  (aw-dispatch-alist
   '((?o aw-swap-window "Swap Windows")
     (?c aw-copy-window "Duplicate the current window")
     (?v aw-split-window-horz "Split horizontally")
     (?s aw-split-window-vert "Split vertically")
     (?p aw-delete-window "Delete Window")
     (?m delete-other-windows "Delete Other Windows")
     (?k akirak/aw-quit-window "Quit window")
     (32 toggle-window-split)
     (?T tear-off-window)
     (?d delete-frame)
     (?f make-frame-command)
     (?? aw-show-dispatch-help)))
  (aw-ignored-buffers '("\\*helm"
                        " *LV*"
                        minibuffer-mode
                        ibuffer-sidebar-mode
                        "*Calc Trail*"))
  (aw-dispatch-always t)
  (aw-ignore-on t))

(defun akirak/ad-around-aw-show-dispatch-help (orig)
  (if (require 'posframe nil t)
      (progn
        (posframe-show "*aw-help*"
                       :string
                       (mapconcat
                        (lambda (action)
                          (cl-destructuring-bind (key fn &optional description) action
                            (format "%s: %s"
                                    (propertize
                                     (char-to-string key)
                                     'face 'aw-key-face)
                                    (or description fn))))
                        aw-dispatch-alist
                        "\n")
                       :poshandler #'posframe-poshandler-frame-bottom-right-corner)
        (advice-add 'aw--done :after (lambda () (posframe-delete "*aw-help*")))
        ;; Prevent this from replacing any help display
        ;; in the minibuffer.
        (let (aw-minibuffer-flag)
          (mapc #'delete-overlay aw-overlays-back)
          (call-interactively 'ace-window)))
    (funcall orig)))

(advice-add 'aw-show-dispatch-help :around 'akirak/ad-around-aw-show-dispatch-help)

(provide 'setup-ace-window)
