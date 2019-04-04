(use-package ace-window
  :custom
  (aw-keys (string-to-list "qwertyui"))
  (aw-background nil)
  ;; Prevent an error from aw-set-make-frame-char
  (aw-make-frame-char nil)
  ;; Ergonomic bindings
  (aw-dispatch-alist
   '((?o aw-swap-window "Swap Windows")
     (?p aw-delete-window "Delete Window")
     (32 toggle-window-split)
     (?d delete-frame)
     (?f make-frame-command)
     (?g magit-status)
     (?z akirak/minor-mode-hydra/body)
     (?a treemacs)
     (?n aweshell-dedicated-open)
     (?s ibuffer-sidebar-toggle-sidebar)
     (?x dired-sidebar-toggle-sidebar)
     (?h windmove-left)
     (?j windmove-down)
     (?k windmove-up)
     (?l windmove-right)
     (?? aw-show-dispatch-help)))
  (aw-ignored-buffers '("\\*helm"
                        " *LV*"
                        minibuffer-mode
                        ibuffer-sidebar-mode))
  (aw-dispatch-always t)
  (aw-scope 'frame)
  (aw-ignore-on t))

(defhydra akirak/minor-mode-hydra (:hint nil)
  "
_d_ debug-on-error [%s(if debug-on-error \"on\" \"off\")]
_f_ focus-mode [%s(if focus-mode \"on\" \"off\")]
_v_ view-mode [%s(if view-mode \"on\" \"off\")]
"
  ("d" toggle-debug-on-error)
  ("f" focus-mode)
  ("v" view-mode)
  ("q" nil "quit" :exit t))

(provide 'setup-ace-window)
