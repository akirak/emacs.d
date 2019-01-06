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
                   ("*org clocking*" :other t)
                   ("*Capture*" :ratio 0.4 :align below)
                   ("*compilation*" :align below :ratio 0.4)
                   ("*lispy-message*" :align below :ratio 0.4)
                   ;; org-capture to org-journal needs a big window
                   ("^CAPTURE-[[:digit:]+]" :regexp t :other t)
                   ("^CAPTURE-\\(code\\)" :regexp t :other t)
                   ("^CAPTURE-\\(journal\\)" :regexp t :other t)
                   ("^CAPTURE-\\(scratch\\)" :regexp t :other t)
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
                   ;; ("*undo tree*" :size 0.2 :align right)
                   ("*Help*" :other t)
                   ("\\*Org Agenda" :regexp t :other t)))
  )

(with-eval-after-load 'org
  (advice-add 'org-switch-to-buffer-other-window
              :override 'switch-to-buffer-other-window))

;;;; Window management rules that can't be configured by shackle
;;;;; org-mode
(setq-default org-agenda-window-setup 'other-window
              org-src-window-setup 'split-window-below
              org-indirect-buffer-display 'current-window)

;;;; Delete compilation window

;; This configuration does not depend on shackle, but I will put it in this file
;; because it is closely related to window management.

;; Based on https://www.reddit.com/r/emacs/comments/8q5uup/close_popwin_on_successful_compilation/e0h8jbi/
(defun akirak/close-compilation-on-finish (buf status)
  (when (string-match "finished" status)
    (message "Compilation successful")
    (run-with-timer 1 nil #'delete-window (get-buffer-window buf))))

(setq-default compilation-finish-functions #'akirak/close-compilation-on-finish)

;; Workaround for a weird behaviour in `org-src-switch-to-buffer'
;; when `org-src-window-setup' is set to `split-window-below'.
;; It splits the window even when exiting the source buffer,
;; which is not what I expect.

(setq-default org-src-window-setup 'split-window-below)

(defvar-local akirak/org-src-last-wconf nil)

(defun akirak/ad-around-org-src-switch-to-buffer (orig buffer context)
  (if (eq org-src-window-setup 'split-window-below)
      (if (memq context '(exit save))
          (let ((wconf (with-current-buffer buffer
                         akirak/org-src-last-wconf)))
            (set-window-configuration wconf)
            (if-let* ((w (get-buffer-window buffer)))
                (select-window w)
              (switch-to-buffer buffer)))
        (let ((wconf (current-window-configuration)))
          (cond
           ((> (window-total-width) 160)
            (split-window-right))
           ((> (window-total-height 30))
            (split-window-below)))
          (other-window 1)
          (switch-to-buffer buffer)
          (setq akirak/org-src-last-wconf wconf)))
    (funcall orig buffer context)))

(advice-add 'org-src-switch-to-buffer :around
            'akirak/ad-around-org-src-switch-to-buffer)

(provide 'setup-window-management)
