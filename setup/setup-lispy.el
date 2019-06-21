(use-package lispy
  :hook
  ((emacs-lisp-mode
    ielm-mode)
   . lispy-mode)
  (minibuffer-setup
   . (lambda ()
       (when (eq this-command 'eval-expression)
         (lispy-mode 1))))
  :general
  (:keymaps 'lispy-mode-map
            ;; Bind M-m to easy-mark (from easy-kill package) instead
            "M-m" nil
            ;; Use outline-insert-heading rather than lispy-meta-return
            [remap lispy-outline-promote] 'outline-promote
            [remap lispy-outline-demote] 'outline-demote
            [remap lispy-meta-return] 'outline-insert-heading))

(defun akirak/lispy-goto-symbol-elisp-other-window (symbol)
  "Goto definition of an Elisp SYMBOL in other window."
  (let (rsymbol)
    (if (null (setq symbol (intern-soft symbol)))
        (error "symbol not interned")
      (cond ((and current-prefix-arg (boundp symbol))
             (find-variable-other-window symbol))
            ((fboundp symbol)
             (condition-case nil
                 (find-function-other-window symbol)
               (error
                (goto-char (point-min))
                (if (re-search-forward (format "^(def.*%S" symbol) nil t)
                    (move-beginning-of-line 1)
                  (lispy-complain
                   (format "Don't know where `%S' is defined" symbol))
                  (pop-tag-mark)))))
            ((boundp symbol)
             (find-variable-other-window symbol))
            ((or (featurep symbol)
                 (locate-library
                  (prin1-to-string symbol)))
             (find-library-other-window (prin1-to-string symbol)))
            ((setq rsymbol
                   (cl-find-if
                    `(lambda (x)
                       (equal (car x)
                              ,(symbol-name symbol)))
                    (lispy--fetch-this-file-tags)))
             ;; If the symbol is in the same file, jump to it in the same window
             (goto-char (aref (nth 4 rsymbol) 0)))
            (t
             (error "Couldn't find definition of %s"
                    symbol))))))

(advice-add 'lispy-goto-symbol-elisp :override #'akirak/lispy-goto-symbol-elisp-other-window)

(provide 'setup-lispy)
