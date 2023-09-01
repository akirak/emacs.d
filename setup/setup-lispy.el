(use-package lispy
  :config
  (advice-add 'hydra-lispy-x/body
              :override
              'akirak/lispy-x-hydra/body)
  :hook
  ((lisp-mode
    emacs-lisp-mode
    lisp-interaction-mode
    ielm-mode
    eval-expression-minibuffer-setup)
   . lispy-mode)
  :general
  ;; Disable keybindings I don't like
  (:keymaps 'lispy-mode-map-lispy
            "C-," nil
            "M-<left>" nil
            "M-<right>" nil
            "M-m" nil
            "<M-return>" nil
            "<M-RET>" nil)
  :custom
  (lispy-key-theme '(special lispy)))

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

(advice-add 'lispy-goto-symbol-elisp
            :override #'akirak/lispy-goto-symbol-elisp-other-window)

(pretty-hydra-define akirak/lispy-x-hydra
  (:title "Lispy code transformation"
          :exit t)
  ("Conditionals"
   (("c" lispy-to-cond "if to cond")
    ("i" lispy-to-ifs "cond to if"))
   "Functions"
   (("l" lispy-to-lambda "defun to lambda")
    ("d" lispy-to-defun "lambda to defun"))
   "Extract/bind"
   (("b" akirak/lispy-bind-variable "Bind in a new let")
    ("D" lispy-extract-defun "Extract defun"))
   "Development"
   (("e" lispy-edebug "edebug"))))

(defun akirak/lispy-bind-variable (name)
  ;; An alternative to `lispy-bind-variable', which seems to mangle undo-fu.
  (interactive "sName: ")
  (-let* (((start . end) (if (region-active-p)
                             (cons (region-beginning) (region-end))
                           (bounds-of-thing-at-point 'sexp)))
          (sexp (buffer-substring start end))
          (keyword (if current-prefix-arg
                       "let*"
                     "let")))
    (delete-region start end)
    (insert "(" keyword " ((" name " " sexp ")\n")
    (let ((pos (point)))
      (insert "\n)\n")
      (push-mark)
      (insert name ")")
      (goto-char pos))))

(provide 'setup-lispy)
