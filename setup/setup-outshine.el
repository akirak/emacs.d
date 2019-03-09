;; (diminish 'outline-minor-mode)
(add-hook 'prog-mode-hook 'outline-minor-mode)

(use-package outshine
  :config
  (defun akirak/set-outshine-level-faces (theme)
    (dolist (i (mapcar #'int-to-string (number-sequence 1 8)))
      (let ((target (intern (concat "outshine-level-" i)))
            (parent (intern (concat "org-level-" i))))
        (custom-theme-set-faces
         theme `(,target ((default :inherit ,parent)))))))
  (eval-after-load 'dracula-theme
    (akirak/set-outshine-level-faces 'dracula))
  :general
  (:keymaps 'outline-minor-mode-map
            "C-M-i" nil)
  :hook
  (outline-minor-mode . outshine-mode)
  :custom
  (outshine-use-speed-commands t))

(provide 'setup-outshine)
