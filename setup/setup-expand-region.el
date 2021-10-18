(use-package puni
  :general
  ("M-m" (general-predicate-dispatch #'puni-mark-sexp-at-point
           (region-active-p)
           #'puni-expand-region)))

(use-package expand-region
  :disabled t
  :general
  ;; This mark behaviour is similar to that of easy-mark command from
  ;; easy-kill package.
  ("M-m" (general-predicate-dispatch #'er/expand-region
           (region-active-p)
           #'er/expand-region
           (looking-at (rx-to-string comment-start))
           #'er/mark-comment
           (looking-at (rx (any "\"'`")))
           #'er/mark-outside-quotes
           (looking-at (rx (any alnum "-_")))
           #'er/mark-symbol-with-prefix)))

(use-package the-org-mode-expansions
  :disabled t
  :straight expand-region
  :after org
  :general
  (:keymaps 'org-mode-map
            "M-m" #'er/expand-region))

(use-package akirak-elec-pair
  :straight (:host github :repo "akirak/trivial-elisps"
                   :files ("akirak-elec-pair.el"))
  :init
  (general-unbind :keymaps 'lispy-mode-map :package 'lispy
    "M-i")
  (general-unbind "M-i")
  :general
  (:prefix "M-i"
           "c" #'akirak-elec-pair-replace
           "d" #'akirak-elec-pair-delete))

;; embrace.el is based on expand-region
(use-package embrace
  ;; embrace depends on expand-region, so I have to stop using it
  :disabled t
  :init
  (general-unbind :keymaps 'lispy-mode-map :package 'lispy
    "M-i")
  :general
  ("M-i" #'embrace-commander))

(provide 'setup-expand-region)
