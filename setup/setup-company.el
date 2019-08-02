(use-package company
  ;; :diminish company-mode
  :general
  (:keymaps 'company-active-map :package 'company
            "C-n" #'company-select-next
            "C-p" #'company-select-previous
            "M-/" #'company-other-backend)
  :hook
  (prog-mode . company-mode)
  :custom
  (company-dabbrev-other-buffers 'all "Search all buffers for company-dabbrev")
  (company-tooltip-align-annotations t)
  (company-backends '(company-capf
                      company-keywords
                      company-yasnippet
                      company-files
                      company-dabbrev)
                    "Remove some backends I am unlikely to use"))

(use-package company-quickhelp
  :after company
  :init
  (company-quickhelp-mode 1))

(use-package company-statistics
  :disabled t
  :after company
  :init
  (company-statistics-mode 1))

(provide 'setup-company)
