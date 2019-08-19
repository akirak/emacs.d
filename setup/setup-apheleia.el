(use-package apheleia
  :straight (apheleia :host github :repo "raxod502/apheleia")
  :config
  (add-to-list 'apheleia-mode-alist '(sgml-mode . prettier))
  (apheleia-global-mode 1))

(provide 'setup-apheleia)
