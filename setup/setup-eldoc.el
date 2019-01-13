;; Turn off global-eldoc-mode, as I use some alternative help systems
;; e.g. lsp-mode, depending on the language.
(global-eldoc-mode -1)

(autoload 'lv-message "lv")
(autoload 'lv-delete-window "lv")

(defvar akirak/eldoc-lv-window nil)

(defun akirak/eldoc-message (message &rest args)
  (apply #'lv-message format args)
  (setq akirak/eldoc-lv-window t))

(defun akirak/eldoc-delete-window (&rest _args)
  (when akirak/eldoc-lv-window
    (lv-delete-window)
    (setq akirak/eldoc-lv-window nil)))

;; Use lv.el (which is in the same repo as hydra) to display
;; eldoc messages.
(setq eldoc-message-function (cl-function
                              (lambda (format &rest args)
                                (if (and format
                                         (not (string-empty-p format)))
                                    (apply #'akirak/eldoc-message format args)
                                  (akirak/eldoc-delete-window)))))

(advice-add #'display-buffer :before #'akirak/eldoc-delete-window)

(provide 'setup-eldoc)
