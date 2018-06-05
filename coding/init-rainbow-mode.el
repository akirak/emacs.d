(use-package rainbow-mode
  :commands (rainbow-mode)
  :hook
  (prog-mode . (lambda () (rainbow-mode 1))))

(provide 'init-rainbow-mode)
