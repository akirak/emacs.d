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

;;;; Main capture templates

(general-setq
 org-starter-initial-capture-templates
 `(("a" "Append plain text to the clocked task" plain
    (clock)
    "%i"
    :empty-lines 1 :immediate-finish t)
   ("i" "Add an item to the clocked task" item
    (clock)
    "%i%?" :empty-lines 1)
   ("t" "Sub-task of the clocked task" entry
    (clock)
    ,(akirak/org-capture-entry-template-1 "%i%?" ""
                                          :todo "TODO")
    :clock-in t :clock-resume t)
   ("n" "Note under the clocked task" entry
    (clock)
    ,(akirak/org-capture-entry-template-1 "%i%?" "")
    :clock-in t :clock-resume t)
   ("p" "Protocol quote" entry (clock)
    ,akirak/org-protocol-note-template)
   ("L" "Protocol link (as item)" item (clock)
    "[[%:link][%:description]] %?")
   ("d" "To the default notes file")
   ("dt" "Task in the default notes file" entry
    (file "")
    ,(akirak/org-capture-entry-template-1 "%i%?" ""
                                          :todo "TODO")
    :clock-in t)
   ("dn" "Note in the default notes file" entry
    (file "")
    ,(akirak/org-capture-entry-template-1 "%i%?" "")
    :clock-in t)
   ("dp" "Protocol quote" entry (file "") ,akirak/org-protocol-note-template
    :clock-in t)
   ("dL" "Protocol link (as entry)" entry (file "") ,akirak/org-protocol-link-template
    :clock-in t)
   ("u" "Urgent task" entry
    (file "")
    "* NEXT %?\nDEADLINE: %t\n%i"
    :clock-in t :clock-resume t)))

;;;; Template contexts
(setq org-capture-templates-contexts
      `(
        ;; Capture into org-default-notes-file when not clocking in
        ,@(cl-loop for key in '("t" "n" "p" "L")
                   collect `(,key ,(concat "d" key)
                                  ((lambda () (not (org-clocking-p))))))
        ;; Disable templates with the clock target when not clocking in
        ("@" (org-clocking-p))
        ,@(cl-loop for (key _ _ target) in org-starter-initial-capture-templates
                   when (equal target '(clock))
                   collect `(,key (org-clocking-p)))))

(provide 'setup-org-capture)
;;; setup-org-capture.el ends here
