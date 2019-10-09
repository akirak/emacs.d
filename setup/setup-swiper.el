(use-package swiper
  :config
  (add-to-list 'swiper-font-lock-exclude 'emacs-lisp-mode)
  (add-to-list 'swiper-font-lock-exclude 'org-mode)
  :general
  ("C-s" #'swiper
   "C-r" #'swiper-backward
   "C-M-s" #'swiper-isearch
   "C-M-r" #'swiper-isearch-backward))

(provide 'setup-swiper)
