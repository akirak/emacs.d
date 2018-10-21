;;; Terminal-related settings

;;;; Configure the default terminal/shell application

;; Use aweshell as the default terminal
(require 'init-aweshell)

;; Map commands

(defalias 'akirak/shell-open-dedicated #'aweshell-dedicated-open
  "Open a dedicated window for the default shell/terminal application.")

(defalias 'akirak/shell-new #'aweshell-new
  "Switch to a new shell buffer in the current window.")

(defalias 'akirak/shell-next #'aweshell-next
  "Switch to the previous shell buffer.")

(defalias 'akirak/shell-previous #'aweshell-prev
  "Switch to the next shell buffer.")

;; Alternatively, you can use multi-term, but I prefer aweshell

;; (require 'init-multi-term)

;;;; frame-workflow

(akirak/define-frame-workflow "terminal"
  :key "t"
  :make-frame
  '(frame-purpose-make-frame :modes '(term-mode
                                      eshell-mode
                                      shell-mode))
  :layout
  '(ibuffer-sidebar-show-sidebar))

(provide 'init-terminal)
