;; Configuration of mmm-mode for React JSX in TypeScript, a.k.a. TSX.
;; Based on https://gist.github.com/rangeoshun/67cb17392c523579bc6cbd758b2315c1
(use-package mmm-mode
  :after (typescript-mode)
  :config

  ;; Add css mode for CSS in JS blocks
  (mmm-add-classes
   '((mmm-styled-mode
      :submode css-mode
      :front "\\(styled\\|css\\)[.()<>[:alnum:]]?+`"
      :back "`;?")))

  (mmm-add-mode-ext-class 'typescript-mode nil 'mmm-styled-mode)

  ;; Add submodule for graphql blocks
  (mmm-add-classes
   '((mmm-graphql-mode
      :submode graphql-mode
      :front "gr?a?p?h?ql`"
      :back "`;")))

  (mmm-add-mode-ext-class 'typescript-mode nil 'mmm-graphql-mode)

  ;; Add JSX submodule, because typescript-mode is not that great at it
  (mmm-add-classes
   '((mmm-jsx-mode
      :front "\\(return\s\\|n\s\\|(\n\s*\\)<"
      :front-offset -1
      :back ">\n?\s*)"
      :back-offset 1
      :submode web-mode)))

  (mmm-add-mode-ext-class 'typescript-mode nil 'mmm-jsx-mode)

  (defun mmm-reapply ()
    (mmm-mode)
    (mmm-mode))

  (add-hook 'mmm-mode-hook
            (defun akirak/mmm-reapply-on-save ()
              (when (eq major-mode 'typescript-mode)
                (add-hook 'after-save-hook
                          #'mmm-reapply
                          nil 'local))))

  :custom
  (mmm-global-mode 'maybe)
  (mmm-submode-decoration-level 0))

(provide 'setup-mmm)
