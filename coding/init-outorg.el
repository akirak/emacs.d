;;; outorg
(use-package outorg
  :general
  (:keymaps 'outorg-edit-minor-mode-map :package 'outorg
            "C-c '" #'outorg-copy-edits-and-exit)
  :custom
  (outorg-edit-buffer-message-body "C-' to exit"))

(provide 'init-outorg)
