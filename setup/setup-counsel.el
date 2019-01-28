(use-package counsel
  ;; :diminish counsel-mode
  :init
  (counsel-mode 1) ; Remap built-in functions with counsel equivalents
  :config
  (global-set-key [remap recentf-open-files] 'counsel-recentf)
  (global-set-key [remap insert-char] 'counsel-unicode-char))

(defun akirak/ad-after-counsel-org-goto-action (_x)
  (org-show-entry))
(advice-add 'counsel-org-goto-action :after
            'akirak/ad-after-counsel-org-goto-action)

(provide 'setup-counsel)
