(use-package dimmer
  :config
  (general-add-hook 'dimmer-buffer-exclusion-regexps
                    (mapcar #'regexp-quote
                            '("*Help*"
                              "*LV*")))
  (dimmer-configure-helm)
  (dimmer-configure-org)
  (dimmer-configure-which-key)
  (dimmer-mode 1))

(provide 'setup-dimmer)
