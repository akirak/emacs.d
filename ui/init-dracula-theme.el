(use-package dracula-theme
  ;; TODO: Extend the theme to support outshine headings
  ;; See https://github.com/dracula/emacs/blob/master/dracula-theme.el
  :config
  (let ((key2 "#0189cc")
        (bg1 "#282a36")
        (bg4 "#565761")
        (type  "#bd93f9"))
    (custom-theme-set-faces
     'dracula
     `(header-line ((default
                      :background ,bg1
                      :foreground ,type
                      :box t)))
     ;; `(outshine-level-1 SPEC
     ;;                    )
     ))
  ;; Add a custom face for the header line so that it looks distinct
  )

;; (with-eval-after-load 'whitespace
;;   (set-face-attribute 'whitespace-tab nil
;;                       :background "#f0f0f0"
;;                       :foreground "#00a8a8"
;;                       :weight 'bold)
;;   (set-face-attribute 'whitespace-trailing nil
;;                       :background "#e4eeff"
;;                       :foreground "#183bc8"
;;                       :weight 'normal))

(provide 'init-dracula-theme)
