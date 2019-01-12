;; Turn off global-eldoc-mode, as I use some alternative help systems
;; e.g. lsp-mode, depending on the language.
(global-eldoc-mode -1)

(autoload 'lv-message "lv")
(autoload 'lv-delete-window "lv")

;; Use lv.el (which is in the same repo as hydra) to display
;; eldoc messages.
(setq eldoc-message-function (cl-function
                              (lambda (format &rest args)
                                (if (and format
                                         (not (string-empty-p format)))
                                    (apply #'lv-message format args)
                                  (lv-delete-window)))))

(provide 'setup-eldoc)
