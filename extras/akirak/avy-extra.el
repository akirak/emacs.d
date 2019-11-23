(defcustom akirak/avy-symbol-regexp "\\_<\\sw"
  "Regular expression for a symbol.")

;;;###autoload
(defun akirak/M-z (key)
  ;; Because interactive "c" doesn't accept escape characters,
  ;; I use `read-char' here.
  (interactive (list (read-key-sequence-vector "Char: ")))
  (unless (equal key [7])
    (push-mark)
    (let* ((char (char-to-string (seq-elt key 0))))
      (pcase current-prefix-arg
        ('(4)
         (avy-with avy-goto-word-1
           (avy-jump (cond
                      ((string= char ".") "\\.")
                      (t (concat "\\_<" (regexp-quote char))))
                     :beg (line-beginning-position)
                     :end (line-end-position))))
        ('(16)
         (avy-with avy-goto-word-1
           (avy-jump (cond
                      ((string= char ".") "\\.")
                      (t (concat "\\_<" (regexp-quote char))))
                     :beg (save-excursion (beginning-of-defun) (point))
                     :end (save-excursion (end-of-defun) (point)))))
        ((or 'nil
             (and (pred numberp)
                  n
                  (guard (> n 0))))
         (let ((pos (progn
                      (forward-char)
                      (re-search-forward char (line-end-position)
                                         t current-prefix-arg))))
           (if pos
               (goto-char (1- pos))
             (pop-mark)
             (user-error "Not found %s after the cursor in the line" char))))
        ((or '-
             (pred numberp))
         (let ((pos (progn
                      (backward-char)
                      (re-search-backward char (line-beginning-position)
                                          t (when (numberp current-prefix-arg)
                                              (- current-prefix-arg))))))
           (if pos
               (goto-char pos)
             (pop-mark)
             (user-error "Not found %s after the cursor in the line" char))))
        (_ (user-error "Unsupported prefix argument %s" current-prefix-arg))))))

;;;###autoload
(defun akirak/avy-goto-defun ()
  "Jump to the beginning of defun."
  (interactive)
  (avy-with akirak/avy-goto-defun
    (let ((avy-all-windows nil))
      (avy-jump (mapconcat (lambda (l)
                             (concat "\\(" (nth 1 l) "\\)"))
                           imenu-generic-expression
                           "\\|"))))
  (back-to-indentation))

;;;###autoload
(defun akirak/avy-goto-symbol-in-window ()
  (interactive)
  (avy-with akirak/avy-goto-symbol-in-window
    (avy-jump akirak/avy-symbol-regexp
              :window-flip t
              :beg (window-start)
              :end (window-end))))

;;;###autoload
(defun akirak/avy-goto-symbol-in-defun ()
  (interactive)
  (avy-with akirak/avy-goto-symbol-in-defun
    (let ((avy-all-windows nil))
      (avy-jump akirak/avy-symbol-regexp
                :beg (save-excursion
                       (beginning-of-defun)
                       (point))
                :end (save-excursion
                       (end-of-defun)
                       (point))))))

;;;###autoload
(defun akirak/insert-symbol (&optional arg)
  (interactive "P")
  (let* ((orig-window (selected-window))
         (orig-pos (point))
         (avy-all-windows t)
         (cands (-flatten-n 1 (--map (with-selected-window it
                                       (avy--regex-candidates "\\_<\\sw"
                                                              (window-start) (window-end)))
                                     (avy-window-list)))))
    (avy-with akirak/insert-symbol
      (avy--process cands
                    (cl-case avy-style
                      (pre #'avy--overlay-pre)
                      (at #'avy--overlay-at)
                      (at-full #'avy--overlay-at-full)
                      (post #'avy--overlay-post))))
    (let* ((begin (point))
           (end (when (looking-at (rx (+ (any word "-_")) (or bos space)))
                  (1- (match-end 0))))
           (token (buffer-substring-no-properties begin end)))
      (kill-new token)
      (select-window orig-window)
      (goto-char orig-pos)
      (akirak/insert-quoted-symbol token))))

(defun akirak/insert-quoted-symbol (token)
  (-let (((open . close)
          ;; Brackets are hard-coded.
          (cond
           ((derived-mode-p 'org-mode) '("=" . "="))
           ((derived-mode-p 'markdown-mode) '("`" . "`"))
           ((and (derived-mode-p 'emacs-lisp-mode)
                 (lispy--in-comment-p))
            '("`" . "'"))
           (t '(nil . nil)))))
    (insert (or open "") token (or close ""))))


;;;; Inline jump
(defun akirak/avy-goto-in-line (regexp)
  (let (beg end)
    (save-excursion
      (beginning-of-line)
      (setq beg (point))
      (end-of-line)
      (setq end (point)))
    (avy-with avy-goto-char-in-line
      (avy--generic-jump regexp t avy-style beg end))))

(defun akirak/avy-goto-symbol-in-line ()
  (interactive)
  (akirak/avy-goto-in-line "\\_<\\sw"))

(defun akirak/avy-goto-word-in-line ()
  (interactive)
  (akirak/avy-goto-in-line "\\b\\sw"))

(defun akirak/avy-goto-quote-in-line ()
  (interactive)
  (akirak/avy-goto-in-line "\'\\S-"))

(defun akirak/avy-goto-dquote-in-line ()
  (interactive)
  (akirak/avy-goto-in-line "\"\\<"))

;;;; Jump to an open bracket in defun
(defun akirak/avy-goto-open-bracket-above-in-defun ()
  (interactive)
  (avy-with avy-goto-char
    (avy--generic-jump "[({\[]" t avy-style
                       (save-excursion (beginning-of-defun) (point))
                       (point))))

(defun akirak/avy-goto-open-bracket-below-in-defun ()
  (interactive)
  (avy-with avy-goto-char
    (avy--generic-jump "[({\[]" t avy-style
                       (1+ (point))
                       (save-excursion (end-of-defun) (point)))))

(provide 'akirak/avy-extra)
