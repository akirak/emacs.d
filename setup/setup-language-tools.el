;;;; Language-agnostic tools

(use-package google-translate
  :commands (google-translate-at-point
             google-translate-query-translate)
  :functions (google-translate-translate)
  :config
  (akirak/bind-language
    "t" #'google-translate-at-point))

;;;; Tools for specific languages

;;;;; Japanese
(use-package katawa
  :straight (:host github :repo "akirak/katawa.el")
  :config
  (akirak/bind-language
    "j" #'katawa-ivy-at-point))

(provide 'setup-language-tools)
