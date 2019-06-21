;; Mostly based on an example in https://github.com/tumashu/pyim/blob/master/README.md
(use-package pyim-basedict
  :config (pyim-basedict-enable))

(use-package pyim
  :custom
  (pyim-default-scheme 'quanpin)
  (pyim-page-tooltip 'popup)
  (pyim-page-length 9)
  (pyim-auto-select t)
  (pyim-english-input-switch-functions '(pyim-probe-isearch-mode
                                         pyim-probe-program-mode
                                         pyim-probe-org-structure-template))
  (pyim-punctuation-half-width-functions '(pyim-probe-punctuation-line-beginning
                                           pyim-probe-punctuation-after-punctuation))
  :config
  (pyim-isearch-mode 1))

(provide 'setup-chinese-pyim)
