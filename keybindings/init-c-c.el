(general-def
  "C-c l" 'org-store-link
  "C-c p" 'yankpad-map
  "C-c v" '(nil :wk "display-buffer")
  "C-c v b" '((lambda () (interactive) (display-buffer "*Backtrace*")) :wk "backtrace")
  "C-c v c" '((lambda () (interactive) (display-buffer "*compilation*")) :wk "compilation")
  "C-c v m" '((lambda () (interactive) (display-buffer "*Messages*")) :wk "messages")
  "C-c v w" '((lambda () (interactive) (display-buffer "*Warnings*")) :wk "warnings")
  "C-c v s" '((lambda () (interactive) (pop-to-buffer "*scratch*")) :wk "scratch")
  "C-c y" 'yankpad-insert)

(provide 'init-c-c)
