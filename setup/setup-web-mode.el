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
         ("\\.djhtml\\'" . web-mode)))

(provide 'setup-web-mode)
