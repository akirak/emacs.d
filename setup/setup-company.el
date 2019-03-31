(use-package company
  :config
  (advice-add 'completion-at-point :override
              #'company-complete-common-or-cycle)
  (defun company-complete-common-or-cycle-backward ()
    "Complete common prefix or cycle backward."
    (interactive)
    (company-complete-common-or-cycle -1))
  :general
  (:keymaps 'company-active-map :package 'company
            ;; "C-f" #'company-complete-selection
            "RET" #'company-complete-selection
            [return] #'company-complete-selection
            "TAB" #'company-complete-common-or-cycle
            "<tab>" #'company-complete-common-or-cycle
            "S-TAB" #'company-complete-common-or-cycle-backward
            "<backtab>" #'company-complete-common-or-cycle-backward
            "C-;" #'company-show-doc-buffer
            "C-M-/" #'company-filter-candidates
            "C-n" #'company-select-next
            "C-p" #'company-select-previous
            "C-o" #'company-other-backend)
  :hook
  (((prog-mode
     minibuffer-setup) . company-mode)
   (minibuffer-setup
    . (lambda () (setq-local company-frontends
                             '(company-preview-if-just-one-frontend))))
   ((shell-mode sh-mode fish-mode eshell-mode)
    . (lambda ()
        (setq-local company-backends '(company-capf company-files)))))
  :custom
  (company-auto-complete nil)
  (company-dabbrev-other-buffers 'all "Search all buffers for company-dabbrev")
  (company-tooltip-align-annotations t)
  (company-backends '(company-capf
                      company-yasnippet
                      company-keywords
                      company-files)
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
