(use-package flyspell
  :diminish 'flyspell-mode
  :hook
  ((text-mode . (lambda () (flyspell-mode 1)))
   ((change-log-mode log-edit-mode) . (lambda () (flyspell-mode -1)))
   (prog-mode . flyspell-prog-mode)))

(require 'init-flyspell)
