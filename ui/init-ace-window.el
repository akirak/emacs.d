(use-package ace-window
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j))
  (aw-background nil)
  (aw-dispatch-alist
   '((?k aw-delete-window "Delete Window")
     (?m aw-swap-window "Swap Windows")
     (?o delete-other-windows "Delete Other Windows")
     (?? aw-show-dispatch-help)))
  (aw-ignored-buffers '("\\*helm" minibuffer-mode))
  (aw-ignore-on t)
  (ace-window-display-mode nil))

(provide 'init-ace-window)
