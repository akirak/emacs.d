;;;; Language-agnostic tools

(use-package google-translate
  :commands (google-translate-at-point
             google-translate-query-translate)
  :functions (google-translate-translate)
  :config
  ;; Changes needed because of changes in the Translate API:
  ;; https://github.com/atykhonov/google-translate/issues/52
  (defun google-translate--search-tkk ()
    "Search TKK." (list 430675 2721866130))
  (setq google-translate-backend-method 'curl))

;;;; Tools for specific languages

;;;;; Japanese
(use-package katawa
  :straight (katawa :host github :repo "akirak/katawa.el")
  :commands (katawa-ivy katawa-ivy-at-point))

(provide 'setup-language-tools)
