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

(general-create-definer akirak/bind-help-key
  :prefix "<menu>")

(global-unset-key (kbd "<menu>"))

(akirak/bind-help-key
  "?" #'akirak/helm-search)

(provide 'init-general)
