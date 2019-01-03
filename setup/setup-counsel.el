(use-package counsel
  :diminish counsel-mode
  :init
  (counsel-mode 1) ; Remap built-in functions with counsel equivalents
  :config
  (global-set-key [remap recentf-open-files] 'counsel-recentf)
  (global-set-key [remap insert-char] 'counsel-unicode-char)
  (ivy-add-actions 'counsel-describe-function
                   '(("j" akirak/counsel--find-symbol-other-window
                      "definition in other window")))
  (ivy-add-actions 'counsel-describe-variable
                   '(("j" akirak/counsel--find-symbol-other-window
                      "definition in other window"))))

(defun find-library-noselect (sym)
  (when-let ((filename (locate-library (prin1-to-string sym))))
    (with-current-buffer (or (get-buffer-visiting filename)
                             (find-file-noselect filename))
      (cons (current-buffer) (point-min)))))

(defun counsel--find-symbol-noselect (x)
  "Find symbol definition that corresponds to string X."
  (with-ivy-window
    (counsel--push-xref-marker)
    (let ((full-name (get-text-property 0 'full-name x)))
      (if full-name
          (find-library-noselect full-name)
        (let ((sym (read x)))
          (cond ((and (eq (ivy-state-caller ivy-last)
                          'counsel-describe-variable)
                      (boundp sym))
                 (find-variable-noselect sym))
                ((fboundp sym)
                 (find-function-noselect sym))
                ((boundp sym)
                 (find-variable-noselect sym))
                (t
                 (or (and (featurep sym)
                          (find-library-noselect sym))
                     (error "Couldn't find definition of %s"
                            sym)))))))))

(defun counsel--find-symbol-other-window (x)
  "Find symbol definition that corresponds to string X."
  (pcase-let ((`(,buf . ,point)) (counsel--find-symbol-noselect x))
    (switch-to-buffer-other-window buf)
    (goto-char point)))

(provide 'setup-counsel)
