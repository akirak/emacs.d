;;; Terminal-related settings

;;;; Configure the default terminal/shell application

(setq akirak/shell-settings
      '((aweshell :open-dedicated aweshell-dedicated-open
                  :new aweshell-new
                  :next aweshell-next
                  :prev aweshell-prev)
        (multi-term :open-dedicated multi-term-dedicated-open
                    :new multi-term
                    :next multi-term-next
                    :prev multi-term-prev)))

(defcustom akirak/default-shell 'multi-term
  "The defaut shell for use."
  :set
  (lambda (key value)
    (set-default key value)
    (let ((settings (alist-get value akirak/shell-settings)))
      ;; Open a dedicated window for the default shell/terminal application.
      (cl-loop for (key value) on settings by #'cddr
               do (let ((name (intern (concat "akirak/shell-"
                                              (string-remove-prefix ":" (symbol-name key))))))
                    (fset name value))))))

(require 'init-multi-term)

;; Map commands

;; (require 'init-multi-term)

;;;; frame-workflow

(akirak/define-frame-workflow "terminal"
  :key "t"
  :make-frame
  '(frame-purpose-make-frame :modes '(term-mode
                                      eshell-mode
                                      shell-mode))
  :layout
  '(ibuffer-sidebar-show-sidebar))

(provide 'init-terminal)
