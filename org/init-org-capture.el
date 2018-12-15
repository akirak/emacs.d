;; This file does not contain an actual configuration of
;; `org-capture-templates'. See init-org-starter.el instead.

;;;; Meta-templates: Templating functions for templates

(defun akirak/babel-capture-template (language &optional
                                               initial-before
                                               initial-after)
  (concat "* TODO %^{Title of the block}
:PROPERTIES:
:CREATED_TIME: %U
:language: " language "
:CAPTURE_ORIGIN: %a
:CAPTURE_CLOCKING: %K
:END:

#+begin_src " language "\n"
(or initial-before "") "%?" (or initial-after "") "
#+end_src

"))

(cl-defun akirak/org-capture-entry-template-1 (&optional heading body
                                                         &key (todo nil)
                                                         (tags nil)
                                                         (extra-props nil))
  (declare (indent 1))
  (concat "* " (if todo (concat todo " ") "") (or heading "%?")
          (when tags
            (format " :%s:" (string-join tags ":")))
          "\n:PROPERTIES:\n:CREATED_TIME: %U\n"
          (cl-etypecase extra-props
            (null "")
            (string extra-props)
            (list (cl-loop for (key . value) in extra-props
                           concat (format ":%s: %s\n"
                                          (cl-etypecase key
                                            (symbol (symbol-name key))
                                            (string key))
                                          value))))
          ":END:\n"
          body
          "\n\n"))

(defconst akirak/org-protocol-note-template
  "* %?
:PROPERTIES:
:CREATED_TIME: %U
:END:

#+BEGIN_QUOTE
%i
#+END_QUOTE

[[%:link][%:description]]

")

(defconst akirak/org-protocol-link-template
  "* [[%:link][%:description]]
:PROPERTIES:
:CREATED_TIME: %U
:END:

%?

")

;;;; Jump to a heading

(defun akirak/org-find-heading-or-prepend (level text)
  "Find or create a heading with the given text at the given level."
  (let ((prefix (concat (make-string (org-get-valid-level level) ?*) " "))
        (bound (unless (= level 1)
                 (save-excursion (org-end-of-subtree)))))
    (unless (re-search-forward (concat "^" (regexp-quote prefix) text)
                               bound t)
      (if (re-search-forward (concat "^" prefix) bound t)
          (end-of-line 0)
        (end-of-line 1))
      (insert "\n" prefix text))))

(defun akirak/org-reverse-date-tree (&optional time)
  "Jump to the specified date in a reverse date tree.

A reverse date tree is a reversed version of the date tree in
`org-capture', i.e. a date tree where the newest date is the first.
This is especially useful for a notes archive, because the latest
entry on a particular topic is displayed at the top in
a command like `helm-org-rifle'.

TIME is the date to be inserted. If omitted, this will be today."
  (let* ((time (or time (current-time))))
    (save-restriction
      (widen)
      (goto-char (point-min))
      (akirak/org-find-heading-or-prepend 1 (format-time-string "%Y" time))
      (akirak/org-find-heading-or-prepend 2 (format-time-string "%Y-%m %B" time))
      (akirak/org-find-heading-or-prepend 3 (format-time-string "%Y-%m-%d %A" time)))))

(provide 'init-org-capture)
;;; init-org-capture.el ends here
