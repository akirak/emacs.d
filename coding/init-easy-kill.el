(use-package easy-kill
  :general
  ([remap kill-ring-save] #'easy-kill
   "M-m" #'easy-mark))

(provide 'init-easy-kill)
