(defun akirak/mix-command-alist ()
  (->> (process-lines "mix" "help")
       (-map (lambda (s)
               (save-match-data
                 (when (string-match (rx bol (group "mix" (* (not (any "#"))))
                                         " # " (group (+ anything)) eol)
                                     s)
                   (cons (string-trim-right (match-string 1 s))
                         (match-string 2 s))))))
       (delq nil)))

(provide 'my/compile/mix)
