(use-package vdiff
  :config
  (akirak/bind-file-extra
    "d" #'vdiff-files)
  :general
  (:keymaps 'vdiff-mode-map
            akirak/major-mode-hydra-key #'vdiff-hydra/body))

(use-package vdiff-magit
  :after magit
  :config
  ;; Just copied from https://github.com/justbur/emacs-vdiff-magit
  (define-key magit-mode-map "e" 'vdiff-magit-dwim)
  (define-key magit-mode-map "E" 'vdiff-magit)
  (transient-suffix-put 'magit-dispatch "e" :description "vdiff (dwim)")
  (transient-suffix-put 'magit-dispatch "e" :command 'vdiff-magit-dwim)
  (transient-suffix-put 'magit-dispatch "E" :description "vdiff")
  (transient-suffix-put 'magit-dispatch "E" :command 'vdiff-magit))

(provide 'setup-vdiff)
