(use-package all-the-icons)

(defun akirak/make-header-line-format (&rest body)
  "Build a header line format with BODY indicating additional information."
  `("  "
    (:eval (or (and (featurep 'all-the-icons)
                    (all-the-icons-icon-for-buffer))
               mode-name))
    " "
    (buffer-file-name ((:eval (file-name-nondirectory buffer-file-name))
                       ": ")
                      (:eval (buffer-name)))
    (:eval (when (buffer-narrowed-p) "<Nrr> "))
    ,@body))

;;;; Default header line with which-function

;; The default header line format with which-function.
(which-function-mode 1)

(defun akirak/set-default-header-line ()
  (setq header-line-format (akirak/make-header-line-format
                            '(which-func-mode ("" which-func-format " ")))))

;; As I don't know how to disable the header line in the which-key window
;; (I tried `which-key-init-buffer-hook', but it didn't work),
;; I turn on the default header line only in certain groups of buffers
;; rather than setting it using `setq-default'.
(add-hook 'find-file-hook #'akirak/set-default-header-line)
(add-hook 'magit-mode-hook #'akirak/set-default-header-line)
(add-hook 'help-mode-hook #'akirak/set-default-header-line)

;;;; Header line formats for specific modes
;;;;; org-mode
;; Set the header line format for org-mode with the outline path.
(setq-mode-local org-mode
                 header-line-format
                 (akirak/make-header-line-format
                  '(:eval (unless (org-before-first-heading-p)
                            (org-format-outline-path
                             (let* ((orig-rev (nreverse (org-get-outline-path t t)))
                                    (seg-length (pcase (length orig-rev)
                                                  ((pred (< 4)) 8)
                                                  ((pred (< 2)) 12)
                                                  (_ 20))))
                               (nreverse
                                (cons (car orig-rev)
                                      (mapcar (lambda (s)
                                                (if (> (length s) seg-length)
                                                    (substring s 0 seg-length)
                                                  s))
                                              (cdr orig-rev))))))))))

;;;;; org-agenda-mode
(defun akirak/set-org-agenda-header-line ()
  (setq header-line-format
        '((:eval (and (featurep 'all-the-icons)
                      (all-the-icons-icon-for-buffer)))
          " "
          (:eval (pcase org-agenda-redo-command
                   (`(org-agenda-run-series ,desc . ,_)
                    (when-let ((key (caar (cl-remove-if-not
                                           (lambda (list) (equal (nth 1 list) desc))
                                           org-agenda-custom-commands))))
                      (format "[%s]%s" key (car (split-string desc ":")))))
                   (x (prin1-to-string x)))))))

(add-hook 'org-agenda-mode-hook #'akirak/set-org-agenda-header-line)

(provide 'init-header-line)
