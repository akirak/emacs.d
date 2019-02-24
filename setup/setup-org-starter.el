(autoload 'helm-org-rifle-files "helm-org-rifle")

(use-package org-starter
  :config
  (org-starter-mode 1)
  (function-put #'org-starter-def-capture 'lisp-indent-function 2)
  (unless (bound-and-true-p org-starter-path)
    (general-setq org-starter-path '("~/org/")))
  :custom
  ;; `org-starter-initial-capture-templates` is defined in setup-org-capture.el
  (org-starter-initial-capture-templates
   '(("a" "Append to the current clock")
     ("c" "cpb.org & code.org")))
  (org-starter-require-file-by-default nil)
  (org-starter-exclude-from-recentf '(known-files path))
  (org-starter-enable-local-variables :all))

(defun helm-org-rifle-known-files ()
  (interactive)
  (helm-org-rifle-files org-starter-known-files))

;;;; Keybindings
(akirak/bind-mode :keymaps 'org-mode-map :package 'org
  "r" #'org-starter-refile-by-key)

;;;; Workflow

;;;;; Generic tasks and notes

(org-starter-def-capture "g" "Generic entry in the inbox (with %i as title)"
  entry (file "scratch.org")
  "* %i%?
:PROPERTIES:
:CREATED_TIME: %U
:END:
"
  :clock-in t :clock-resume t :empty-lines 1)

(org-starter-def-capture "t" "Scheduled task (with %i as title)"
  entry (file "taskpool.org")
  "* TODO %i%?
DEADLINE: %^{Deadline}T SCHEDULED: %^{When to do}t
:PROPERTIES:
:CREATED_TIME: %U
:END:
"
  :clock-in t :clock-resume t :empty-lines 1)

(org-starter-def-capture "u" "Urgent task (with %i as title)"
  entry (file "taskpool.org")
  "* STARTED %i%?
DEADLINE: %T SCHEDULED: %T
:PROPERTIES:
:CREATED_TIME: %U
:END:
"
  :clock-in t :clock-resume t)

;;;; Appending contents to the current clock

;;;; Commonplace book

;; These capture templates are mostly based on ones by alphapapa:
;; https://www.reddit.com/r/emacs/comments/9ycgoe/do_you_guys_have_a_personal_wiki_using_orgmode_or/ea0j8nw/

(org-starter-def-capture "cp" "cpb.org: Plain entry (with %i as body)"
  entry (file+function "cpb.org" org-reverse-datetree-goto-date-in-file)
  "* %^{Heading}
:PROPERTIES:
:CREATED_TIME: %U
:END:

%(unless (string-empty-p \"%i\") \"%i\n\n\")%?"
  :clock-in t :clock-resume t :empty-lines 1)

(org-starter-def-capture "cq" "cpb.org: Quote"
  entry (file+function "cpb.org" org-reverse-datetree-goto-date-in-file)
  "* %(if (string-empty-p \"%a\") \"%^{Heading}\" \"%a\")
:PROPERTIES:
:CREATED_TIME: %U
:END:

%?%(unless (string-empty-p \"%x\") \"

#+BEGIN_QUOTE
%x
#+END_QUOTE
\")"
  :clock-in t :clock-resume t :empty-lines 1)

(org-starter-def-capture "cl" "cpb.org: Link to web page"
                         entry (file+function "cpb.org" org-reverse-datetree-goto-date-in-file)
                         "* %(org-web-tools--org-link-for-url) :@link:
:PROPERTIES:
:CREATED_TIME: %U
:END:

%?"
                         :clock-in t :clock-resume t :empty-lines 1)

(org-starter-def-capture "cr" "cpb.org: Readable content of web page"
  entry (file+function "cpb.org" org-reverse-datetree-goto-date-in-file)
  "%(org-web-tools--url-as-readable-org)"
  :clock-in t :clock-resume t :empty-lines 1)

;;;;; Misc
(org-starter-def-capture "j" "journal.org: New entry (with %i as body)"
                         entry (file+function "journal.org" org-reverse-datetree-goto-read-date-in-file)
                         "* %^{Title}
:PROPERTIES:
:CREATED_TIME: %U
:END:
%(unless (string-empty-p \"%i\") \"%i\n\n\")%?"
                         :clock-in t :clock-resume t :empty-lines 1)

(org-starter-def-capture "co" "code.org: Stub entry (with %i as body)"
  entry (file+function "code.org" org-reverse-datetree-goto-read-date-in-file)
  "* %^{Title}
:PROPERTIES:
:CREATED_TIME: %U
:END:
"
  :clock-in t :clock-resume t :empty-lines 1)

;;;; Resources
;;;;; org-capture
;;- [[http://www.howardism.org/Technical/Emacs/capturing-content.html][Capturing Content for Emacs]]
;;- [[https://github.com/alphapapa/org-web-tools][alphapapa/org-web-tools]]
;;- [[https://github.com/alphapapa/org-protocol-capture-html][alphapapa/org-protocol-capture-html]]
;;- [[https://www.reddit.com/r/emacs/comments/9ycgoe/do/][Do you guys have a personal wiki? (using org-mode or not) : emacs]]
;;  - [[https://www.reddit.com/r/emacs/comments/9ycgoe/do_you_guys_have_a_personal_wiki_using_orgmode_or/ea0j8nw/][Example configuration by alphapapa]]
;;- [[https://github.com/abo-abo/orca][GitHub - abo-abo/orca: ORg CApture]]

(provide 'setup-org-starter)
