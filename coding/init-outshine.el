(diminish 'outline-minor-mode)
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
  :custom
  (outshine-use-speed-commands t))

(use-package navi-mode
  :disabled t)

;; Jumping based on outlines (better than imenu)
(use-package helm-navi :after (helm navi-mode)
  :disabled t)

(provide 'init-outshine)
