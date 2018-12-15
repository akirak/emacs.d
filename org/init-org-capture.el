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

(provide 'init-org-capture)
;;; init-org-capture.el ends here
