(use-package dimmer
  :straight (:type built-in)
  ;; :unless (akirak/windows-subsystem-for-linux-p)
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
