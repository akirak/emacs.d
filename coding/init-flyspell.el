(use-package flyspell
  :diminish 'flyspell-mode
  :init
  ;; Turn on flyspell in text-mode
  (add-hook 'text-mode-hook (lambda () (flyspell-mode 1)))
  ;; Turn off flyspell in these modes
  (dolist (hook '(change-log-mode-hook log-edit-mode-hook))
    (add-hook hook (lambda () (flyspell-mode -1))))
  ;; Turn on flyspell in comments in prog-mode
  (add-hook 'prog-mode-hook 'flyspell-prog-mode))

(require 'init-flyspell)
