(use-package web-mode
  :mode (("\\.erb\\'" . web-mode)
         ("\\.mustache\\'" . web-mode)
         ;; ("\\.html?\\'" . web-mode)
         ("\\.php\\'" . web-mode)
         ;; ("\\.vue\\'" . web-mode)
         ("\\.jsp\\'" . web-mode)
         ;; ("\\.jsx?$" . web-mode)
         ("\\.es6\\'" . web-mode)
         ("\\.ejs\\'" . web-mode)
         ("\\.phtml\\'" . web-mode)
         ("\\.tpl\\.php\\'" . web-mode)
         ("\\.[agj]sp\\'" . web-mode)
         ("\\.as[cp]x\\'" . web-mode)
         ("\\.djhtml\\'" . web-mode))
  :config
  ;; which-key descriptions for prefixes
  (pcase-dolist (`(,prefix ,desc)
                 '(("C-c C-t" "web-mode tag")
                   ("C-c C-e" "web-mode element")
                   ("C-c C-d" "web-mode dom")
                   ("C-c C-b" "web-mode block")
                   ("C-c C-a" "web-mode attribute")))
    (which-key-add-keymap-based-replacements web-mode-map prefix desc))
  :custom
  (web-mode-code-indent-offset 2)
  (web-mode-markup-indent-offset 2))

(provide 'setup-web-mode)
