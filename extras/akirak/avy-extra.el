(defcustom akirak/avy-symbol-regexp "\\_<\\sw"
  "Regular expression for a symbol.")

;;;###autoload
(defun akirak/avy-goto-defun ()
  "Jump to the beginning of defun."
  (interactive)
  (avy-with akirak/avy-goto-defun
    (let ((avy-all-windows nil))
      (avy--generic-jump (mapconcat (lambda (l)
                                      (concat "\\(" (nth 1 l) "\\)"))
                                    imenu-generic-expression
                                    "\\|")
                         nil avy-style
                         nil nil)))
  (back-to-indentation))

;;;###autoload
(defun akirak/avy-goto-symbol-in-window ()
  (interactive)
  (avy-with akirak/avy-goto-symbol-in-window
    (avy--generic-jump akirak/avy-symbol-regexp
                       t avy-style (window-start) (window-end))))

;;;###autoload
(defun akirak/avy-goto-symbol-in-defun ()
  (interactive)
  (avy-with akirak/avy-goto-symbol-in-defun
    (let ((avy-all-windows nil))
      (avy--generic-jump akirak/avy-symbol-regexp
                         nil avy-style
                         (save-excursion
                           (beginning-of-defun)
                           (point))
                         (save-excursion
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

(provide 'akirak/avy-extra)