(general-def :keymaps 'swiper-map :package 'swiper
  ;; Allow dispatching occur from a swiper session.
  "C-M-o" (defun akirak/swiper-occur ()
            (interactive)
            (ivy-exit-with-action
             (lambda (_) (occur ivy-text)))))

(general-def :keymaps 'counsel-ag-map :package 'counsel
  ;; Allow dispatching noccur from a counsel-rg session.
  "C-M-o" (defun akirak/counsel-ag-noccur ()
            (interactive)
            (ivy-exit-with-action
             (lambda (_) (deadgrep ivy-text)))))

(use-package noccur
  :disabled t)

(use-package deadgrep
  :commands (deadgrep))

(use-package comb
  :commands (comb))

(provide 'setup-occur)
