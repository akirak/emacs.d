(when (version<= "26.1" emacs-version)
  (defun turn-on-display-line-numbers-mode ()
    (interactive)
    (display-line-numbers-mode 1))
  (add-hook 'prog-mode-hook #'turn-on-display-line-numbers-mode)
  (add-hook 'text-mode-hook #'turn-on-display-line-numbers-mode))

(provide 'init-line-numbers)
