(use-package dracula-theme
  ;; TODO: Extend the theme to support outshine headings
  ;; See https://github.com/dracula/emacs/blob/master/dracula-theme.el
  :config
  (let ((key2 "#0189cc")
        (bg1 "#282a36")
        (bg2 "#373844")
        (bg4 "#565761")
        (fg2 "#e2e2dc")
        (fg4 "#b6b6b2")
        (const "#8be9fd")
        (type "#bd93f9"))
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
                                   :foreground ,fg2))))))

(provide 'init-dracula-theme)
