(akirak/bind-key key-translation-map
  ;; * Obsolete
  ;; As <menu> (application on Windows keyboards) is hard to reach on some
  ;; keyboards, I will use <C-tab> instead. This key combination is occupied on
  ;; web browsers but vacant on most Emacs major modes, so it is safe to use it
  ;; on non-EXWM buffers.
  ;; "<C-tab>" (kbd "<menu>")

  ;; Chromebook don't have physical function keys. They substitute
  ;; Search + num for function keys, but Search + 1 is hard to press,
  ;; especially when Search and Ctrl are swapped.
  ;; This is quite annoying, so I will use M-` as <f1>.
  "M-`" (kbd "<f1>"))

(define-globalized-minor-mode akirak/emulate-chromeos-mode
  nil
  (lambda ()
    (cond
     (akirak/emulate-chromeos-mode
      (dolist (n (number-sequence 1 9))
        (define-key key-translation-map
          (kbd (format "s-%d" n)) (kbd (format "<f%d>" n))))
      (define-key key-translation-map
        (kbd "s-0") (kbd "<f10>"))
      (define-key key-translation-map
        (kbd "s--") (kbd "<f11>"))
      (define-key key-translation-map
        (kbd "s-=") (kbd "<f12>")))
     (t
      (dolist (n (number-sequence 0 9))
        (define-key key-translation-map
          (kbd (format "s-%d" n)) nil))))))

(unless (akirak/running-on-crostini-p)
  (akirak/emulate-chromeos-mode 1))

(provide 'setup-key-translation)
