;; Use general.el to define keybindings. It has made several improvements over
;; bind-key, including a built-in support for which-key.
;;
;; Use `general-def' to define keybindings.

(use-package general)

(defmacro akirak/define-contextual-key (key &rest general-defs)
  (declare (indent 1))
  (macroexp-progn
   (cl-loop for (def . context) in general-defs
            collect (cons 'general-define-key
                          (append context
                                  (list key def))))))

(general-create-definer akirak/bind-help-key
  :prefix "<menu>")

(provide 'init-general)
