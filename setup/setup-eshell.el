(use-package eshell
  :straight nil
  :general
  (:keymaps 'eshell-mode-map
            "C-r" #'eshell-isearch-backward))

(defun akirak/eshell-dedicated ()
  (interactive)
  (split-window-below)
  (other-window 1)
  (eshell)
  (set-window-dedicated-p (selected-window) t))

(use-package esh-help
  :preface
  (autoload 'esh-help-eldoc-command "esh-help")
  (defun esh-help-turn-on ()
    (interactive)
    (setq-local eldoc-documentation-function
                'esh-help-eldoc-command)
    (setq eldoc-documentation-function
          'esh-help-eldoc-command)
    (eldoc-mode 1))
  :hook (eshell-mode . esh-help-turn-on))

(use-package em-dired
  )

(provide 'setup-eshell)
