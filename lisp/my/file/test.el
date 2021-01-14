;; -*- lexical-binding: t; -*-
(defvar-local akirak/test-and-impl-file-local-rules nil)

(defcustom akirak/test-and-impl-file-rules
  '(("\\.el\\'"
     (test->impl "-tests?\\.el\\'" :replace ".el")
     (test->impl "/tests?/\\([^/]\\)\\.el\\'" :replace "/$1")
     (impl->test "/\\([^/]+\\)\\.el\\'"
                 :replace
                 "/$1-test.el" "/$1-tests.el"
                 "/test/$1.el" "/tests/$1.el"))
    ("\\.tsx?\\'"
     (test->impl "\\.\\(spec\\|test\\)\\(\\.tsx?\\)\\'"
                 :replace "$2")
     (impl->test "\\(\\.tsx?\\)\\'"
                 :replace ".test$1" ".spec$1")))
  "FIXME"
  :type 'sexp)

(defun akirak/possible-test-or-impl-file (file rules)
  (declare (indent 1))
  (-some->> rules
    (-find (lambda (rule) (string-match-p (car rule) file)))
    (cdr)
    (-some (pcase-lambda (`(,name ,pattern ,verb . ,args))
             (save-match-data
               (when-let (n (string-match pattern file))
                 (let ((prefix (substring file 0 n))
                       (matches (cl-loop for (start end) on (match-data) by #'cddr
                                         collect (substring file start end))))
                   (cons name
                         (pcase verb
                           (:replace
                            (-map (lambda (replace)
                                    (concat prefix
                                            (s-format replace 'elt matches)))
                                  args)))))))))))

(defun akirak/select-test-or-impl-file ()
  (when-let (file (buffer-file-name))
    (when (project-current)
      (if-let (local-matches (akirak/possible-test-or-impl-file file
                               akirak/test-and-impl-file-local-rules))
          (cadr local-matches)
        (when-let (matches (akirak/possible-test-or-impl-file file
                             akirak/test-and-impl-file-rules))
          (or (-find #'f-exists-p (cdr matches))
              (completing-read (format "%s: " (car matches))
                               (-map #'f-short (cdr matches)))))))))

(provide 'my/file/test)
