(use-package apheleia
  :straight (apheleia :host github :repo "raxod502/apheleia")
  :config
  (add-to-list 'apheleia-mode-alist '(sgml-mode . prettier)))

(provide 'setup-apheleia)
