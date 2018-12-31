(use-package switch-window
  :straight (switch-window :host github :repo "akirak/switch-window")
  :init
  ;; A wrapper function for `switch-window' command with prefix support
  (defun akirak/switch-window (&optional arg)
    "`switch-window' with an optional prefix argument.

By default, this function behaves the same as `switch-window'.

When a universal prefix is given, select a window to delete."
    (interactive "P")
    (pcase arg
      ('(4) (switch-window-then-delete))
      (_ (switch-window))))
  :config
  (defun akirak/switch-window-frame-list-function ()
    "Frame list function for `switch-window' on EXWM."
    (cl-remove-if-not #'exwm-workspace--active-p (visible-frame-list)))
  (with-eval-after-load 'exwm-input
    (setq switch-window-frame-list-function
          'akirak/switch-window-frame-list-function))
  :general
  (:keymaps 'switch-window-extra-map
            "q" #'keyboard-quit)
  :custom
  (switch-window-multiple-frames t)
  (switch-window-shortcut-style 'qwerty)
  ;; Set the window characters
  ;; t is used to toggle floating in EXWM
  ;; q is used to quit
  (switch-window-qwerty-shortcuts
   (mapcar #'char-to-string (string-to-list "asdfgwer")))
  (switch-window-minibuffer-shortcut ?z)
  (switch-window-input-style 'minibuffer))

(provide 'init-switch-window)
