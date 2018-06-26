(unless (require 'dash-functional nil t)
  (use-package dash-functional))

(use-package frame-purpose
  :straight (frame-purpose :host github :repo "alphapapa/frame-purpose.el")
  :config
  (frame-purpose-mode 1))

(use-package frame-workflow
  :straight (frame-workflow :host github :repo "akirak/frame-workflow")
  :config
  (frame-workflow-mode 1))

(use-package helm-frame-workflow
  :after (frame-workflow helm)
  :straight frame-workflow
  :commands (helm-frame-workflow))

;;;; Some workspaces
(akirak/define-frame-workflow "emacs-config"
  :layout '(find-file (expand-file-name "init.el" user-emacs-directory))
  :make-frame '(frame-purpose-make-directory-frame user-emacs-directory))

;;;; Keymap to switch to a frame

(define-prefix-command 'akirak/frame-map)

(defun akirak/frame-workflow-bind (&optional bindings)
  (let ((prefixes (mapcar 'key-description (where-is-internal 'akirak/frame-map))))
    (cl-loop for (key . name) in (or bindings akirak/frame-workflow-bindings)
             do (define-key akirak/frame-map (kbd key)
                  `(lambda () (interactive) (frame-workflow-switch-frame ,name)))
             do (cl-loop for prefix in prefixes
                         do (which-key-add-key-based-replacements
                              (concat prefix " " key) (concat "" name))))))

;; Store keybindings in the custom file, as my workflow is defined in the file.
(defcustom akirak/frame-workflow-bindings nil
  "Alist of keybindings in `akirak/frame-map' to switch to a workspace."
  :group 'frame-workflow
  :group 'akirak
  :type '(repeat (cons (string :tag "Key")
                       (string :tag "Subject")))
  :set (lambda (symbol value)
         (set-default symbol value)
         (akirak/frame-workflow-bind value)))

;; As the prefix map is not bound to any key and the prefix key is unknown at
;; the time of loading this file, add the which-key replacements after startup.
(add-hook 'emacs-startup-hook 'akirak/frame-workflow-bind)

(provide 'init-frames)
