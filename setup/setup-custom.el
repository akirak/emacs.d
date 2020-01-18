;;;; Managing host-specific configuration
(defgroup akirak/local
  '((treemacs-persist-file custom-variable)
    (git-identity-list custom-variable)
    (org-starter-path custom-variable)
    (akirak/org-wiki-default-dir custom-variable))
  "Variables that should be specific to machines."
  :group 'akirak)

(defun akirak/setup-custom-file ()
  (interactive)
  (setq custom-file (read-file-name "Custom file: "
                                    (when custom-file (file-name-directory custom-file))
                                    custom-file))
  (load-file custom-file)
  (find-file "~/.emacs-profiles.el")
  (customize-group-other-window 'akirak/local))

(defun akirak/check-custom-file ()
  (cond
   ((and (bound-and-true-p custom-file)
         (file-equal-p custom-file "~/.custom.el"))
    (when (yes-or-no-p "The current value is custom-file is the default, \
which is likely to be wrong. Do you want to change the value and start machine-specific customization?")
      (akirak/setup-custom-file)))
   ((null custom-file)
    (message "Custom-file is not set."))))

(add-hook 'emacs-startup-hook 'akirak/check-custom-file)

;;;; Utilities for customizations
(defun akirak/symbols-with-property (property)
  (let (faces)
    (mapatoms
     (lambda (sym)
       (when (get sym property)
         (push sym faces))))
    (nreverse faces)))

(defun akirak/face-list ()
  (akirak/symbols-with-property 'face-defface-spec))

(defun akirak/custom-variable-list ()
  (akirak/symbols-with-property 'custom-type))

(cl-defun akirak/ivy-file-variable (&key initial-input)
  (interactive)
  (ivy-read "Custom variables with file type: "
            (let (faces)
              (mapatoms
               (lambda (sym)
                 (pcase (get sym 'custom-type)
                   ((or 'file
                        'directory
                        `(file . ,_))
                    (push (propertize (symbol-name sym)
                                      'symbol-value (ignore-errors
                                                      (symbol-value sym))
                                      'documentation (ignore-errors
                                                       (get sym 'variable-documentation)))
                          faces)))))
              (nreverse faces))
            :action (lambda (input)
                      (let ((file (symbol-value (intern-soft input))))
                        (cond
                         ((null file)
                          (message "Variable %s has no value" input))
                         ((string-empty-p file)
                          (message "Variable %s is an empty string" input))
                         ((file-directory-p file)
                          (let ((default-directory (file-name-as-directory file)))
                            (counsel-find-file)))
                         ((and (file-name-absolute-p file)
                               (file-exists-p file))
                          (if (file-executable-p file)
                              ;; TODO: What should I do on executables?
                              (dired-jump nil file)
                            (find-file file)))
                         ((executable-find file)
                          (shell-command))
                         (t
                          (message "File %s is neither existent nor executable" file)))))
            :initial-input initial-input
            :caller 'akirak/ivy-file-variable))

(defun akirak/executables-in-path ()
  (cl-loop for dir in exec-path
           with result = nil
           when (file-directory-p dir)
           append (thread-last (directory-files dir 'full nil 'nosort)
                    (cl-remove-if-not #'file-executable-p)
                    (mapcar #'file-name-nondirectory)
                    (cl-remove-if (lambda (name) (string-prefix-p "." name))))
           into result
           finally return (cl-remove-duplicates (cl-sort result #'string<)
                                                :test #'string-equal)))

(with-eval-after-load 'ivy
  (ivy-set-actions
   'akirak/ivy-file-variable
   '(("d" dired-jump-other-window "dired-jump")
     ("I" (lambda (inp)
            (insert (symbol-value inp)))
      "Insert value")
     ("s" (lambda (x)
            (let* ((type (read-char-choice
                          (format "Edit variable %s [p: path, x: executable, o: other]: "
                                  x)
                          (string-to-list "pxo")))
                   (sym (intern-soft x))
                   (value (cl-case type
                            (?p (read-file-name (format "File path for %s: " x)
                                                nil (symbol-value sym)))
                            (?x (completing-read (format "Executable for %s: " x)
                                                 (akirak/executables-in-path)))
                            (?o (read-string (format "Value for %s: " x)))))
                   (to-save (yes-or-no-p (format "Save the value of %s? " x))))
              (if to-save
                  (customize-save-variable sym value
                                           "Set by ivy-file-variable.")
                (customize-set-variable sym value
                                        "Set by ivy-file-variable."))))
      "Set a value"))))

(defun akirak/documentation-first-line (str)
  (if str
      (let ((pos (string-match
                  (rx (or (eval sentence-end) (any "\n\r"))) str)))
        (if pos
            (substring str 0 (1- pos))
          str))
    ""))

(with-eval-after-load 'ivy
  (ivy-set-display-transformer
   'akirak/ivy-file-variable
   (defun akirak/ivy-file-variable-display-transformer (input)
     (format "%-30s  %s %s"
             (propertize input 'face 'font-lock-variable-name-face)
             (let ((value (get-char-property 0 'symbol-value input)))
               (if (and (stringp value) (not (string-empty-p value)))
                   (format "(%s)" (abbreviate-file-name value))
                 ""))
             (or (ignore-errors
                   (propertize
                    (akirak/documentation-first-line
                     (get-char-property 0 'documentation input))
                    'face 'font-lock-doc-face))
                 "")))))

(akirak/bind-customization "f" #'akirak/ivy-file-variable)

(provide 'setup-custom)
