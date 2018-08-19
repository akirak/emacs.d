(use-package aweshell
  :straight (aweshell :host github :repo "manateelazycat/aweshell")
  :init
  (defcustom aweshell-dedicated-height 20
    "Height of dedicated aweshell windows.")
  (defun aweshell-dedicated-open ()
    (interactive)
    (split-window nil aweshell-dedicated-height 'above)
    (aweshell-new)
    (set-window-dedicated-p nil t)))

(provide 'init-aweshell)
