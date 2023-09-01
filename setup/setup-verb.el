(use-package simple-httpd)

(use-package verb
  :after org
  :config
  (define-key org-mode-map (kbd "C-c C-x M-r") verb-command-map)
  (general-def :keymaps 'org-mode-map
    "C-c C-x M-r" '(nil :wk "verb"))
  :custom
  (verb-base-headers
   '(("User-Agent" . "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/51.0.2704.103 Safari/537.36"))))

(provide 'setup-verb)
