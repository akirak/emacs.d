(general-def :prefix-map 'akirak/ui-map
  :prefix-command 'akirak/ui-map
  "b" 'frame-purpose-show-sidebar
  "d" 'toggle-debug-on-error
  "l" 'imenu-list
  "s" 'symbol-overlay-remove-all
  "z" 'olivetti-mode
  "<f8>" #'dired-sidebar-toggle-sidebar
  ;; The prefix is <f8>, but as <S-f8> is translated to <f8>, so
  ;; you can use toggle this sidebar by pressing <S-f8> two times.
  "<S-f8>" #'ibuffer-sidebar-toggle-sidebar)

(provide 'init-ui-map)
