(defvar akirak/mix-command-alist-cache nil)

(defun akirak/mix-command-alist ()
  (or (alist-get default-directory akirak/mix-command-alist-cache
                 nil nil #'string-equal)
      (let ((result (->> (process-lines "mix" "help")
                         (-map (lambda (s)
                                 (save-match-data
                                   (when (string-match (rx bol (group "mix" (* (not (any "#"))))
                                                           " # " (group (+ anything)) eol)
                                                       s)
                                     (cons (string-trim-right (match-string 1 s))
                                           (match-string 2 s))))))
                         (delq nil))))
        (push (cons default-directory result)
              akirak/mix-command-alist-cache)
        result)))

(provide 'my/compile/mix)
