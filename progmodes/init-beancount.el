(defcustom akirak/ledger-primary-currency "JPY"
  "The currency primarily used in the country I am living in.")

(use-package beancount
  :straight (beancount :host github
                       :repo "beancount/beancount"
                       :files ("editors/emacs/*.el"))
  :mode ("\\.bean\\'" . beancount-mode)
  :custom
  (beancount-mode-map-prefix (kbd "<menu>"))
  (beancount-use-ido nil)
  :init
  (defun akirak/remove-id-from-beancount-capture ()
    (with-current-buffer (marker-buffer org-capture-last-stored-marker)
      (when (bound-and-true-p beancount-mode)
        (when (org-entry-delete org-capture-last-stored-marker "ID")
          (save-buffer)))))
  (add-hook 'org-capture-after-finalize-hook
            #'akirak/remove-id-from-beancount-capture))

(provide 'init-beancount)
