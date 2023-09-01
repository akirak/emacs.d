(use-package tagedit
  :general
  (:keymaps 'tagedit-mode-map
            [remap kill-line] #'tagedit-kill
            [remap down-list] #'tagedit-goto-tag-content)
  ;; (:keymaps 'tagedit-mode-map :prefix "C-. t"
  ;;           "" '(nil :wk "tagedit")
  ;;           ;; These keybindings are experimental.
  ;;           "b" #'tagedit-forward-barf-tag
  ;;           "j" #'tagedit-forward-slurp-tag
  ;;           "r" #'tagedit-raise-tag
  ;;           "s" #'tagedit-splice-tag
  ;;           "J" #'tagedit-join-tags
  ;;           "S" #'tagedit-split-tag
  ;;           "C" #'tagedit-convolute-tags)
  :hook
  ((sgml-mode web-mode) . tagedit-mode))

(provide 'setup-tagedit)
