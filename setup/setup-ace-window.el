(use-package ace-window
  :config
  (custom-theme-set-faces 'user
                          '(aw-leading-char-face
                            ((default
                               :background "gray18" :foreground "tan"
                               :height 250))))
  (advice-add 'aw-delete-window
              :after
              (defun akirak/ad-after-aw-delete-window (&rest _args)
                (balance-windows)))
  :custom
  (aw-keys (string-to-list "qwertyui"))
  (aw-background nil)
  ;; Prevent an error from aw-set-make-frame-char
  (aw-make-frame-char nil)
  ;; Ergonomic bindings
  (aw-dispatch-alist
   '((?o aw-swap-window "Swap Windows")
     (?p aw-delete-window "Delete Window")
     (?m delete-other-windows "Delete Other Windows")
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
  (aw-scope 'frame)
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
