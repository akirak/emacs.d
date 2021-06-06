(use-package go-mode
  :config
  (add-hook 'go-mode-hook
            (defun akirak/go-setup ()
              (add-hook 'before-save-hook #'gofmt nil t))))

(provide 'setup-go)
