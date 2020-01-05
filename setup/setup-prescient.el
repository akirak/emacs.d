(use-package prescient
  :config
  (prescient-persist-mode 1))

(use-package ivy-prescient
  ;; You must load counsel before ivy-prescient
  ;; See https://github.com/raxod502/prescient.el/tree/12ad508c447625918b4d0d93214a6f92f77d5dad#usage
  :after (ivy counsel)
  :config
  (ivy-prescient-mode t)
  ;; Explicitly disable sorting by ivy-prescient for these commands.
  ;; See https://github.com/raxod502/prescient.el/issues/38
  (general-add-hook 'ivy-sort-functions-alist
                    '((ivy-omni-org . nil)
                      (swiper . nil)
                      (org-starter-swiper-config-files . nil)
                      (counsel-minibuffer-history . nil)
                      (counsel-mark-ring . nil)))
  (general-add-hook 'ivy-sort-functions-alist
                    '((akirak/org-capture-bookmark-destination
                       . ivy-prescient-sort-function)))
  ;; Don't fallback to ivy-prescient.
  (general-remove-hook 'ivy-sort-functions-alist
                       '((t . ivy-prescient-sort-function))))

(use-package company-prescient
  :after company
  :hook
  (company-mode . company-prescient-mode))

(provide 'setup-prescient)
