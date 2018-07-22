;;;; Template groups

(let ((non-file-templates `(("t" "Task")
                            ("r" "Reference, reply, comment, etc.")
                            ("i" "Idea")
                            ("s" "Search")
                            ("g" "Goodies")
                            ("o" "Workflow")
                            ("w" "Writing")
                            ("." "Contextual"))))
  (if (null org-capture-templates)
      (setq org-capture-templates non-file-templates)
    ;; Remove duplicates
    (cl-delete-duplicates org-capture-templates :test #'equal :key #'car)
    ;; Insert the template groups
    (dolist (entry non-file-templates)
      (if-let ((existing (assoc (car entry) org-capture-templates)))
          (setcdr existing (cdr entry))
        (push entry org-capture-templates))))
  (cl-sort org-capture-templates #'string< :key #'car))

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

# Add a description

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
            (format " :%s:" (string-join tag ":")))
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
          (if body (concat body "\n") "")
          "\n\n"))

(provide 'init-org-capture)
;;; init-org-capture.el ends here
