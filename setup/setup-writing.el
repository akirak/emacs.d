;;;; Spell checking
(use-package flyspell
  :if (executable-find "ispell")
  :config
  (general-unbind :keymaps 'flyspell-mode-map :package 'flyspell
    "C-," "C-." "C-M-i" "C-c $" "C-;")
  :custom
  (flyspell-abbrev-p t)
  (flyspell-use-global-abbrev-table-p t))

;;;; Quotation
(use-package typo)

;;;; Counting words
(use-package wc-mode
  ;; TODO: Configure modelines
  )

(use-package org-wc
  :after org)

;;;; Writegood
(use-package writegood-mode)

(provide 'setup-writing)
