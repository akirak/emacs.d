;; Use general.el to define keybindings. It has made several improvements over
;; bind-key, including a built-in support for which-key.
;;
;; Use `general-def' to define keybindings.

;; Hopefully, defrepeater is soon going to be integrated with general
(use-package defrepeater)

(use-package general
  :config
  (general-define-key
   [remap other-window] (defrepeater #'other-window)
   [remap winner-undo] (defrepeater #'winner-undo)
   [remap winner-redo] (defrepeater #'winner-redo)
   [remap text-scale-increase] (defrepeater #'text-scale-increase)
   [remap text-scale-decrease] (defrepeater #'text-scale-decrease)))

(defmacro akirak/define-contextual-key (key &rest general-defs)
  (declare (indent 1))
  (macroexp-progn
   (cl-loop for (def . context) in general-defs
            collect (cons 'general-define-key
                          (append context
                                  (list key def))))))

;;;; Help keys

;; Define a prefix for help commands.
(general-create-definer akirak/bind-help-key
  :prefix "<menu>")

(global-unset-key (kbd "<menu>"))

(akirak/bind-help-key
  "?" #'akirak/helm-search)

;;;; Mode-specific keys

;; Define a prefix for commands specific to a major mode.
;; For now, it is the same as that of help commands.
;; To prevent conflicts, use upper-case tail keys for these commands.
(general-create-definer akirak/bind-mode-key
  :prefix "<menu>")

(provide 'init-general)
