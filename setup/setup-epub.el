(use-package nov
  :mode (("\\.epub\\'" . nov-mode))
  :general
  (:keymaps 'nov-mode-map :package 'nov
            "?" #'akirak/nov-mode-hydra/body))

(defhydra akirak/nov-mode-hydra (:exit nil
                                       :hint nil
                                       :foreign-keys 'run)
  "nov-mode"
  ("[" nov-previous-document "previous")
  ("]" nov-next-document "next")
  ("t" nov-goto-toc "toc")
  ("q" nil "quit hydra" :exit t))

(provide 'setup-epub)
