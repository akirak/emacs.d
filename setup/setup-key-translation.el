(general-def key-translation-map
  ;; As <menu> (application on Windows keyboards) is hard to reach on some
  ;; keyboards, I will use <C-tab> instead. This key combination is occupied on
  ;; web browsers but vacant on most Emacs major modes, so it is safe to use it
  ;; on non-EXWM buffers.
  "<C-tab>" (kbd "<menu>")

  ;; Chromebook don't have physical function keys. They substitute
  ;; Search + num for function keys, but Search + 1 is hard to press,
  ;; especially when Search and Ctrl are swapped.
  ;; This is quite annoying, so I will use M-` as <f1>.
  "M-`" (kbd "<f1>"))

(provide 'setup-key-translation)
