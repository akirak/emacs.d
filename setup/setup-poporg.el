(use-package poporg
  :disalbed t)
(akirak/bind-user "'" 'poporg-dwim)
;; The default keybindings in poporg-mode-map are not intuitive to me,
(akirak/bind-key :keymaps 'poporg-mode-map
  "C-c C-c" 'poporg-edit-exit
  "C-x C-s" 'poporg-update-and-save)

(provide 'setup-poporg)
