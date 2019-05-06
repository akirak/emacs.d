(use-package dracula-theme
  :config
  (let ((key2 "#0189cc")
        (bg0 "#000000")
        (bg1 "#282a36")
        (bg2 "#373844")
        (bg4 "#565761")
        (fg2 "#e2e2dc")
        (fg4 "#b6b6b2")
        (const "#8be9fd")
        (type "#bd93f9")
        (keyword "#ff79c6"))
    (custom-theme-set-faces
     'dracula
     `(header-line ((default
                      :background ,bg1
                      :foreground ,type
                      :box t)))
     `(line-number ((default
                      :background ,bg2
                      :foreground ,fg4)))
     `(line-number-current-line ((default
                                   :inherit 'line-number
                                   :bold t
                                   :foreground ,fg2)))
     `(hl-line
       ((default :background ,bg0)))
     `(dired-filter-group-header
       ((default :foreground ,keyword))))))

(provide 'setup-dracula-theme)
