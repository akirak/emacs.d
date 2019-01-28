(defvar akirak/call-interactively-verbose nil
  "When non-nil, `call-interactively' prints its argument.")

(defun akirak/ad-before-call-interactively (name &rest args)
  (when akirak/call-interactively-verbose
    (if args
        (princ (cons name args))
      (princ name))))
(advice-add 'call-interactively :before 'akirak/ad-before-call-interactively)

(defun akirak/funcall-verbosely (function &rest args)
  (if args
      (princ (cons function args))
    (princ function))
  (apply function args))

(defun akirak/ad-around-verbose-call-interactively (orig &rest args)
  (let ((akirak/call-interactively-verbose t))
    (apply orig args)))

(defun akirak/ctrl-meta-f ()
  (interactive)
  (cond
   ((bound-and-true-p smartparens-mode)
    (let* ((thing (sp-get-thing))
           (op (sp-get thing :op)))
      (if (and (not (string-empty-p op))
               (string-equal op (thing-at-point 'char)))
          (call-interactively 'sp-down-sexp)
        (let ((sp-navigate-interactive-always-progress-point t))
          (call-interactively 'sp-next-sexp)))))
   (t (forward-sexp))))
(advice-add 'akirak/ctrl-meta-f
            :around 'akirak/ad-around-verbose-call-interactively)
(general-def "C-M-f" 'akirak/ctrl-meta-f)

(defun akirak/ctrl-meta-b ()
  (interactive)
  (cond
   ((bound-and-true-p smartparens-mode)
    (progn
      (message "Go to the closest sexp beginning before the point")
      (goto-char (sp-get (sp-get-thing t) :beg))))
   (t (call-interactively 'backward-sexp))))
(advice-add 'akirak/ctrl-meta-b
            :around 'akirak/ad-around-verbose-call-interactively)
(general-def "C-M-b" 'akirak/ctrl-meta-b)

(defun akirak/ctrl-meta-p ()
  (interactive)
  (cond
   ((derived-mode-p 'outline-mode)
    (call-interactively 'outline-previous-visible-heading))
   ((bound-and-true-p alchemist-mode)
    (call-interactively 'alchemist-goto-jump-to-previous-def-symbol))
   ((derived-mode-p 'python-mode)
    (call-interactively 'python-nav-backward-defun))
   ((bound-and-true-p smartparens-mode)
    (sp-backward-sexp))))
(advice-add 'akirak/ctrl-meta-p
            :around 'akirak/ad-around-verbose-call-interactively)
(general-def "C-M-p" 'akirak/ctrl-meta-p)

(defun akirak/ctrl-meta-n ()
  (interactive)
  (cond
   ((derived-mode-p 'outline-mode)
    (call-interactively 'outline-next-visible-heading))
   ((bound-and-true-p alchemist-mode)
    (call-interactively 'alchemist-goto-jump-to-next-def-symbol))
   ((derived-mode-p 'python-mode)
    (call-interactively 'python-nav-forward-defun))
   ((bound-and-true-p smartparens-mode)
    (call-interactively 'sp-next-sexp))))
(advice-add 'akirak/ctrl-meta-n
            :around 'akirak/ad-around-verbose-call-interactively)
(general-def "C-M-n" 'akirak/ctrl-meta-n)

(defun akirak/jump-to-end-of-context ()
  (interactive)
  (cond
   ((derived-mode-p 'org-mode)
    (akirak/funcall-verbosely 'org-end-of-subtree))
   ((bound-and-true-p smartparens-mode)
    (let ((thing (sp-get-thing)))
      (if (string-equal "(" (sp-get thing :op))
          (goto-char (sp-get thing :end))
        (call-interactively 'sp-up-sexp))))))
(advice-add 'akirak/jump-to-end-of-context
            :around 'akirak/ad-around-verbose-call-interactively)
(akirak/bind-jump "e" (defrepeater 'akirak/jump-to-end-of-context))

(defun akirak/org-beginning-of-body ()
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in org-mode"))
  (goto-char (org-with-wide-buffer
              (org-back-to-heading)
              (org-element-property :contents-begin
                                    (org-element-headline-parser
                                     (save-excursion
                                       (org-end-of-subtree))))))
  (while (org-at-drawer-p)
    (goto-char (org-element-property :end
                                     (org-element-drawer-parser nil nil)))))

(defun akirak/jump-to-beginning-of-context ()
  (interactive)
  (cond
   ((derived-mode-p 'python-mode)
    (call-interactively 'python-nav-beginning-of-statement))
   ((derived-mode-p 'org-mode)
    (call-interactively 'akirak/org-beginning-of-body))
   ((bound-and-true-p smartparens-mode)
    (call-interactively 'sp-beginning-of-sexp))))
(advice-add 'akirak/jump-to-beginning-of-context
            :around 'akirak/ad-around-verbose-call-interactively)
(akirak/bind-jump "b" (defrepeater 'akirak/jump-to-beginning-of-context))

(defun akirak/ctrl-meta-u ()
  (interactive)
  (cond
   ((derived-mode-p 'python-mode)
    (call-interactively 'python-nav-backward-up-list))
   ((derived-mode-p 'org-mode)
    (call-interactively 'org-up-element))
   ((bound-and-true-p smartparens-mode)
    (call-interactively 'sp-backward-up-sexp))))
(advice-add 'akirak/ctrl-meta-u
            :around 'akirak/ad-around-verbose-call-interactively)
(general-def "C-M-u" 'akirak/ctrl-meta-u)

(provide 'setup-navigation-bindings)