(require 'init-company)

(defcustom akirak/ledger-primary-currency "JPY"
  "The currency primarily used in the country I am living in.")

(use-package company-ledger
  :after company
  :straight (company-ledger :host github
                            :repo "debanjum/company-ledger"))

(use-package beancount
  :straight (beancount :host github
                       :repo "beancount/beancount"
                       :files ("editors/emacs/*.el"))
  :mode ("\\.bean\\'" . beancount-mode)
  :custom
  (beancount-mode-map-prefix (kbd "<menu>"))
  (beancount-use-ido nil)
  :config
  (add-hook 'company-backends 'company-ledger-backend))

(provide 'init-beancount)
