(use-package ace-window
  :custom
  (aw-keys '(?a ?s ?d ?f ?g))
  (aw-background nil)
  (aw-dispatch-alist
   '((?m aw-swap-window "Swap Windows")
     (?x aw-delete-window "Delete Window")
     (?c aw-split-window-horz "Split horizontally")
     (?v aw-split-window-vert "Split vertically")
     (?b balance-windows)
     (?r toggle-window-split)
     (?o delete-other-windows)
     (?F make-frame)
     (?D delete-frame)
     (?z iconify-frame)
     (?? aw-show-dispatch-help)))
  (aw-ignored-buffers '("\\*helm" minibuffer-mode))
  (aw-dispatch-always t)
  (aw-scope 'frame)
  (aw-ignore-on t))

(provide 'init-ace-window)
