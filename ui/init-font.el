;; Setting the default font
;;
;; This solution seem to work for now, but may be changed in the future.
;; https://superuser.com/questions/692173/emacs-default-font-does-not-work-with-new-frames
(defun akirak/set-default-font ()
  (when (member "Hack" (font-family-list))
    (set-default-font "Hack-10.5" nil t)))

(add-hook 'emacs-startup-hook 'akirak/set-default-font)

;; This doesn't allow setting a size
;; (set-face-attribute 'default nil :family "Hack")
(provide 'init-font)
