(use-package move-dup
  :general
  (:keymaps 'move-dup-mode-map
            "M-p"   #'move-dup-move-lines-up
            "M-n"   #'move-dup-move-lines-down
            "C-M-p" #'move-dup-duplicate-up
            "C-M-n" #'move-dup-duplicate-down)
  :hook
  (prog-mode . move-dup-mode)
  :config
  (add-hook 'text-mode-hook
            (defun akirak/turn-on-move-dup-mode ()
              (unless (derived-mode-p 'org-mode 'markdown-mode)
                (move-dup-mode 1)))))

(provide 'setup-edit)
