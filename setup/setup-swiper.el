(use-package swiper
  :config
  (add-to-list 'swiper-font-lock-exclude 'emacs-lisp-mode)
  (add-to-list 'swiper-font-lock-exclude 'org-mode)
  (when (and (not (fboundp 'swiper-isearch-backward))
             (yes-or-no-p "swiper-isearch is unavailable. Would you want to upgrade swiper? "))
    (straight-pull-package "swiper")
    (straight-rebuild-package "swiper")
    (load "swiper"))
  :general
  ("C-s" #'swiper
   "C-r" #'swiper-backward
   "C-M-s" #'swiper-isearch
   "C-M-r" #'swiper-isearch-backward))

(provide 'setup-swiper)
