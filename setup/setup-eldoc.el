;; Turn off global-eldoc-mode, as I use some alternative help systems
;; e.g. lsp-mode, depending on the language.
(global-eldoc-mode -1)

;; I prefer displaying help for Emacs Lisp symbols in lv window,
;; so I won't enable this package for now.
(use-package eldoc-box
  :disabled t
  :config
  (eldoc-box-hover-at-point-mode -1))

(autoload 'lv-message "lv")
(autoload 'lv-delete-window "lv")

(defvar akirak/eldoc-lv-window nil)

(defun akirak/eldoc-message (message &rest args)
  (let ((window (setq akirak/eldoc-lv-window (lv-window)))
        (buffer (get-buffer " *LV*")))
    (unless (eql buffer (get-buffer-window buffer))
      (set-window-buffer window buffer))
    (apply #'lv-message format args)))

(defun akirak/eldoc-delete-window (&rest _args)
  (when akirak/eldoc-lv-window
    (lv-delete-window)
    (setq akirak/eldoc-lv-window nil)))

(defun akirak/eldoc-message-lv (format &rest args)
  (let ((initial-window (selected-window)))
    (unwind-protect
        (if (and format
                 (not (string-empty-p format)))
            (apply #'akirak/eldoc-message format args)
          (akirak/eldoc-delete-window))
      (select-window initial-window))))

(byte-compile #'akirak/eldoc-message-lv)

;; Use lv.el (which is in the same repo as hydra) to display
;; eldoc messages.
(setq eldoc-message-function #'akirak/eldoc-message-lv)

(advice-add #'display-buffer :before #'akirak/eldoc-delete-window)

(defun akirak/turn-on-eldoc-mode ()
  (interactive)
  (eldoc-mode 1))

(provide 'setup-eldoc)
