(use-package shackle
  :init
  (shackle-mode 1)
  :custom
  (shackle-default-rules '(:select t))
  (shackle-default-ratio 0.4)
  (shackle-default-alignment 'below)
  (shackle-rules '(
                   ("\\*ivy-occur counsel-projectile " :regexp t :align left :ratio 0.15)
                   ;; Shackle rules for org-mode
                   ("*Org Select*" :ratio 0.25 :align below)
                   ;; ("\\*Org Src " :regexp t :align below :ratio 0.5)
                   ("\\*Org todo*" :regexp t :ratio 0.15 :align above)
                   ("*Capture*" :ratio 0.4 :align below)
                   ("*compilation*" :ratio 0.25 :align below)
                   ;; org-capture to org-journal needs a big window
                   ("^CAPTURE-[[:digit:]+]" :regexp t :other t)
                   ("^CAPTURE-\\(snippets\\)" :regexp t :other t)
                   ("^CAPTURE-" :regexp t :ratio 0.3 :align below)
                   ;; This should precede the generic helm rule
                   ("*helm top*" :same t)
                   ("*helm-descbinds*" :other t)
                   ("\\*helm.*\\*" :regexp t :ratio 0.25 :align below)
                   ("*Messages*" :align below :ratio 0.3 :noselect t)
                   ("*Warnings*" :align below :ratio 0.3 :noselect t)
                   ("*Backtrace*" :align below :ratio 0.4 :noselect t)
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

;;;; Window management rules that can't be configured by shackle
;;;;; org-mode
(setq-default org-agenda-window-setup 'other-window
              org-src-window-setup 'split-window-below)

;;;; Delete compilation window

;; This configuration does not depend on shackle, but I will put it in this file
;; because it is closely related to window management.

;; Based on https://www.reddit.com/r/emacs/comments/8q5uup/close_popwin_on_successful_compilation/e0h8jbi/
(defun akirak/close-compilation-on-finish (buf status)
  (when (string-match "finished" status)
    (message "Compilation successful")
    (run-with-timer 1 nil #'delete-window (get-buffer-window buf))))

(setq-default compilation-finish-functions #'akirak/close-compilation-on-finish)

(provide 'init-shackle)
