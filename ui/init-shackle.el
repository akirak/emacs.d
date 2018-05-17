(use-package shackle
  :init
  (shackle-mode 1)
  :custom
  (shackle-default-rules '(:select t))
  (shackle-default-ratio 0.4)
  (shackle-default-alignment 'below)
  (shackle-rules '(
                   ;; Shackle rules for org-mode
                   ("*Org Select*" :ratio 0.25 :align below)
                   ;; ("\\*Org Src " :regexp t :align below :ratio 0.5)
                   ("\\*Org todo*" :regexp t :ratio 0.15 :align above)
                   ("*compilation*" :other t)
                   ;; org-capture to org-journal needs a big window
                   ("^CAPTURE-[[:digit:]+]" :regexp t :other t)
                   ("^CAPTURE-\\(fix\\|search\\)" :regexp t :other t)
                   ("^CAPTURE-" :regexp t :ratio 0.3 :align below)
                   ;; This should precede the generic helm rule
                   ("*helm top*" :same t)
                   ("*helm-descbinds*" :other t)
                   ("\\*helm.+\\*" :regexp t :ratio 0.25 :align below)
                   ("*Messages*" :align below :ratio 0.25 :noselect t)
                   ("*Warnings*" :align below :ratio 0.25 :noselect t)
                   ("*Backtrace*" :align below :ratio 0.2 :noselect t)
                   ("\\*Agenda Commands\\*" :regexp t :align below :ratio 0.4)
                   ("*Calendar*" :align below :ratio 0.3)
                   ("*Org Links*" :ratio 0.1 :align below)
                   ;; FIXME: Somehow this doesn't work. Maybe not using display-buffer?
                   ("*undo tree*" :size 0.2 :align right)
                   ;; TODO: Perhaps windows for these buffers should be handled by purpose-mode
                   ("*scratch*" :align below :ratio 0.4 :select t)
                   ("*Help*" :other t)
                   ("\\*Org Agenda" :regexp t :other t))))

(with-eval-after-load 'org
  (advice-add 'org-switch-to-buffer-other-window
              :override 'switch-to-buffer-other-window))

(provide 'init-shackle)
