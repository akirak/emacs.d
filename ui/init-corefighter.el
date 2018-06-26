;;; init-corefighter.el --- corefighter -*- lexical-binding: t -*-

(use-package corefighter
  :straight (corefighter :host github :repo "akirak/corefighter.el")
  :commands (corefighter-sidebar)
  :config
  (corefighter-load-modules))

(provide 'init-corefighter)
;;; init-corefighter.el ends here
