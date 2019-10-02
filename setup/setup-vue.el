(use-package vue-mode
  ;; Turn on vue-mode manually only when necessary
  ;; :mode "\\.vue\\'"
  :general
  (:keymaps 'vue-mode-map
            "C-c '" #'vue-mode-edit-indirect-at-point))

(provide 'setup-vue)
