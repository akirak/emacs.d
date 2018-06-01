(use-package gpastel
  :straight (gpastel :host github :repo "DamienCassou/gpastel")
  :functions (gpastel-start-listening)
  :hook
  (exwm-init-hook . gpastel-start-listening))

(require 'init-helm)

(defun helm-exwm-show-kill-ring ()
  "A variant of `helm-show-kill-ring' that supports pasting to a EXWM buffer."
  (interactive)
  ;; Based on an example in https://github.com/DamienCassou/gpastel
  (let ((inhibit-read-only t)
        ;; Make sure we send selected yank-pop candidate to
        ;; clipboard:
        (yank-pop-change-selection t))
    (call-interactively #'helm-show-kill-ring))
  (when (derived-mode-p 'exwm-mode)
    ;; https://github.com/ch11ng/exwm/issues/413#issuecomment-386858496
    (exwm-input--set-focus (exwm--buffer->id (window-buffer (selected-window))))
    (exwm-input--fake-key ?\C-v)))

(general-def "M-y" #'helm-exwm-show-kill-ring)
;; TODO: Check if this unbinding really works
(general-unbind :keymaps 'counsel-mode-map :package 'counsel [remap yank-pop])

(provide 'init-gpastel)
