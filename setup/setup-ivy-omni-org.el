(use-package ivy-omni-org
  :straight (ivy-omni-org :host github :repo "akirak/ivy-omni-org")
  :custom
  (ivy-omni-org-file-sources '(org-starter-known-files)))

(advice-add 'ivy-omni-org-default-buffer-transformer
            :around
            #'ivy-omni-org--ad-filthy-rich-buffer-transformer)

(defun ivy-omni-org--ad-filthy-rich-buffer-transformer (orig inp)
  (if (bound-and-true-p ivy-filthy-rich-mode)
      (ivy-filthy-rich--format-candidate
       inp
       ivy-filthy-rich-default-switch-buffer-format)
    (funcall orig inp)))

(akirak/bind-register-map "M-o" #'ivy-omni-org)

(provide 'setup-ivy-omni-org)