;; (diminish 'outline-minor-mode)
(add-hook 'prog-mode-hook 'outline-minor-mode)

(use-package outshine
  :config
  (dolist (i (mapcar #'int-to-string (number-sequence 1 8)))
    (let ((target (intern (concat "outshine-level-" i)))
          (parent (intern (concat "org-level-" i))))
      (custom-theme-set-faces
       'dracula `(,target ((default :inherit ,parent))))))
  :general
  (:keymaps 'outline-minor-mode-map
            "C-M-i" nil)
  :hook
  (outline-minor . outshine-mode)
  :custom
  (outshine-use-speed-commands t))

(provide 'setup-outshine)
