(use-package beancount
  ;; Install beancount.el using nix.
  :straight (:type built-in)
  :mode ("\\.beancount\\'" . beancount-mode)
  :custom
  (beancount-mode-map-prefix (kbd akirak/mode-prefix-key))
  (beancount-use-ido nil)
  :init
  (defun akirak/remove-id-from-beancount-capture ()
    (with-current-buffer (marker-buffer org-capture-last-stored-marker)
      (when (bound-and-true-p beancount-mode)
        (when (org-entry-delete org-capture-last-stored-marker "ID")
          (save-buffer)))))
  (add-hook 'org-capture-after-finalize-hook
            #'akirak/remove-id-from-beancount-capture))

(provide 'setup-beancount)
