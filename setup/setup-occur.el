(general-def :keymaps 'swiper-map :package 'swiper
  ;; Allow dispatching occur from a swiper session.
  "C-M-o" (defun akirak/swiper-occur ()
            (interactive)
            (ivy-exit-with-action
             (lambda (_) (occur ivy-text)))))

(use-package noccur
  :general
  ;; Allow dispatching noccur from a counsel-rg session.
  (:keymaps 'counsel-ag-map :package 'counsel
            "C-M-o" (defun akirak/counsel-ag-noccur ()
                      (interactive)
                      (ivy-exit-with-action
                       (lambda (_)
                         (noccur-project ivy-text nil default-directory))))))

(provide 'setup-occur)
