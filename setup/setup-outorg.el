;;; outorg
(use-package outorg
  :general
  (:keymaps 'outorg-edit-minor-mode-map :package 'outorg
            "C-c '" #'outorg-copy-edits-and-exit))

(provide 'setup-outorg)
