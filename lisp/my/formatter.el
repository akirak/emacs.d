(require 'my/project)

(defcustom akirak/project-formatter-list nil
  "List of formatters to use in individual projects.")

(defun akirak/get-reformatter-formatters ()
  (cl-loop for sym being the symbols of obarray
           ;; Filter minor modes
           when (and (memq sym minor-mode-list)
                     (string-suffix-p "-on-save-mode" (symbol-name sym)))
           collect (string-remove-suffix "-on-save-mode" (symbol-name sym))))

(defun akirak/pick-mode-formatter (mode)
  (-some->> akirak/project-formatter-list
    (assoc mode)
    (cdadr)))

(cl-defun akirak/get-project-formatter (&optional project &key mode)
  (let* ((project (or project (akirak/project-root default-directory)))
         (mode (or mode major-mode))
         (formatter (-some->> akirak/project-formatter-list
                      (assoc mode)
                      (cdr)
                      (assoc project)
                      (cdr))))
    (or formatter
        (let* ((formatter (list 'reformatter
                                (completing-read (format "Formatter for %s in project %s: "
                                                         mode
                                                         (abbreviate-file-name project))
                                                 (akirak/get-reformatter-formatters)
                                                 nil t nil nil
                                                 (akirak/pick-mode-formatter mode))))
               (cell (assoc mode akirak/project-formatter-list))
               (subcell (cons project formatter)))
          (cond
           (cell
            (setcdr cell (cons subcell (cdr cell))))
           (t
            (add-to-list 'akirak/project-formatter-list (cons mode (list subcell)))))
          (customize-save-variable 'akirak/project-formatter-list
                                   akirak/project-formatter-list
                                   "Set by akirak/get-project-formatter")
          formatter))))

(provide 'my/formatter)
