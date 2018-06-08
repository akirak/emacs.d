;; As <menu> (application on Windows keyboards) is hard to reach on some
;; keyboards, I will use <C-tab> instead. This key combination is occupied on
;; web browsers but vacant on most Emacs major modes, so it is safe to use it
;; on non-EXWM buffers.
(define-key key-translation-map (kbd "<C-tab>") (kbd "<menu>"))

(provide 'init-key-translation)
