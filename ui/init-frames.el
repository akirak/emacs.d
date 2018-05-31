(unless (require 'dash-functional nil t)
  (use-package dash-functional))

(use-package frame-purpose
  :straight (frame-purpose :host github :repo "alphapapa/frame-purpose.el")
  :config
  (frame-purpose-mode 1))

(use-package frame-workflow
  :straight (frame-workflow :host github :repo "akirak/frame-workflow")
  :config
  (require 'frame-workflow-purpose)
  (frame-workflow-purpose-setup)
  (require 'frame-workflow-menu))

;;;; Keymap to switch to a frame

(define-prefix-command 'akirak/frame-map)

;; Store keybindings in the custom file, as my workflow is defined in the file.
(defcustom akirak/frame-workflow-bindings nil
  "Alist of keybindings in `akirak/frame-map' to switch to a workspace."
  :type '(alist :key-type string :value-type symbol))

(defun akirak/frame-workflow-bind ()
  (let ((prefixes (mapcar 'key-description (where-is-internal 'akirak/frame-map))))
    (cl-loop for (key . symbol) in akirak/frame-workflow-bindings
             do (define-key akirak/frame-map (kbd key)
                  `(lambda () (interactive) (frame-workflow-switch-frame (quote ,symbol))))
             do (cl-loop for prefix in prefixes
                         do (which-key-add-key-based-replacements
                              (concat prefix " " key) (concat "" (symbol-name symbol)))))))

;; As the prefix map is not bound to any key and the prefix key is unknown at
;; the time of loading this file, add the which-key replacements after startup.
(add-hook 'emacs-startup-hook 'akirak/frame-workflow-bind)

(provide 'init-frames)
