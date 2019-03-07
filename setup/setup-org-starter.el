(autoload 'helm-org-rifle-files "helm-org-rifle")

(use-package org-starter
  :straight (org-starter :host github :repo "akirak/org-starter"
                         :branch "config-files")
  :config
  (org-starter-mode 1)
  (unless (bound-and-true-p org-starter-path)
    (general-setq org-starter-path '("~/org/")))
  :custom
  (org-starter-load-config-files t)
  ;; `org-starter-initial-capture-templates` is defined in setup-org-capture.el
  (org-starter-initial-capture-templates
   '(("a" "Append to the current clock")
     ("c" "cpb.org & code.org")
     ("t" "Task")))
  (org-starter-require-file-by-default nil)
  (org-starter-exclude-from-recentf '(known-files path))
  (org-starter-enable-local-variables :all))

(use-package org-reverse-datetree)

(defun helm-org-rifle-known-files ()
  (interactive)
  (helm-org-rifle-files org-starter-known-files))

;;;; Keybindings
(akirak/bind-mode :keymaps 'org-mode-map :package 'org
  "r" #'org-starter-refile-by-key)

;;;; Helper macros and functions

(cl-defmacro akirak/def-org-reverse-datetree-refile (file
                                                     &rest args
                                                     &key key prefer
                                                     &allow-other-keys)
  "Define a refile function as well as a keybinding."
  (declare (indent 1))
  (let* ((basename (file-name-base file))
         (name (intern (concat "akirak/org-refile-to-" basename))))
    `(defun ,name (arg)
       (interactive "P")
       (org-reverse-datetree-refile-to-file
        (org-starter-locate-file ,file nil t) nil
        :prefer ,(or prefer '("CREATED_TIME" "CREATED_AT" "CLOSED"))
        ,@args))
    `(when (quote ,key)
       (add-to-list 'org-starter-extra-refile-map
                    '(,key ,name ,basename)))))

(defun akirak/buffer-mode-name (filename)
  "Return the current major mode name without \"-mode\".

Used in source blocks."
  (with-current-buffer (find-buffer-visiting filename)
    (string-remove-suffix "-mode" (symbol-name major-mode))))
;;;; Refiling
(add-to-list 'org-starter-extra-refile-map
             '("?" akirak/org-refile-same-buffer "same buffer") t)

;;;; Workflow

;;;;; Basic task templates

(org-starter-def-capture "g" "Generic entry in the inbox (with %i as title)"
  entry (file "scratch.org")
  "* %i%?
:PROPERTIES:
:CREATED_TIME: %U
:END:
"
  :clock-in t :clock-resume t :empty-lines 1)

(org-starter-def-capture "tt" "Task without schedule"
  entry (file "taskpool.org")
  "* TODO %^{Heading}
:PROPERTIES:
:CREATED_TIME: %U
:END:

%i%?
"
  :clock-in t :clock-resume t :empty-lines 1)

(org-starter-def-capture "ts" "Scheduled task (with %i as title)"
  entry (file "taskpool.org")
  "* TODO %^{Heading}
DEADLINE: %^{Deadline}T SCHEDULED: %^{When to do}t
:PROPERTIES:
:CREATED_TIME: %U
:END:

%i%?
"
  :clock-in t :clock-resume t :empty-lines 1)

(org-starter-def-capture "tu" "Urgent task (with %i as title)"
  entry (file "taskpool.org")
  "* STARTED %^{Heading} @urgent
DEADLINE: %^{Deadline}T SCHEDULED: %t
:PROPERTIES:
:CREATED_TIME: %U
:END:

%i%?
"
  :clock-in t :clock-resume t)

;;;; Appending contents to the current clock

(org-starter-def-capture "as" "Source block"
  plain (clock)
  "#+BEGIN_SRC %(akirak/buffer-mode-name \"%F\")
%i
#+END_SRC" :empty-lines 1 :immediate-finish t :no-save t)

(org-starter-def-capture "al" "Link"
  plain (clock)
  "%A"
  :empty-lines 1 :immediate-finish t :no-save t)

(org-starter-def-capture "ai" "Item"
  item (clock)
  "%i%?"
  :unnarrowed t :no-save t)

(org-starter-def-capture "ac" "Check item"
  checkitem (clock)
  "[ ] %i%?"
  :unnarrowed t :no-save t)

(org-starter-def-capture "at" "Entry"
  entry (clock)
  "* %?"
  :unnarrowed t :clock-in t :clock-resume t :empty-lines 1 :no-save t)

(org-starter-def-capture "ae" "Example"
  plain (clock)
  "#+begin_example
%i
#+end_example"
  :empty-lines 1 :immediate-finish t :no-save t)

(org-starter-def-capture "aa" "Text"
                         plain (clock)
                         "%?"
                         :empty-lines 1 :unnarrowed t :no-save t)

(org-starter-def-capture "aw" "Web page")

(org-starter-def-capture "aww" "Plain link"
  plain (clock)
  "%(org-web-tools--org-link-for-url)"
  :immediate-finish t :no-save t)

(org-starter-def-capture "awi" "Link as item"
  item (clock)
  "%(org-web-tools--org-link-for-url)"
  :immediate-finish t :no-save t :empty-lines 1)

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
;;;;;; journal.org
(org-starter-def-capture "j" "journal.org: New entry (with %i as body)"
  entry (file+function "journal.org" org-reverse-datetree-goto-read-date-in-file)
  "* %^{Title}
:PROPERTIES:
:CREATED_TIME: %U
:END:

%(unless (string-empty-p \"%i\") \"%i\n\n\")%?"
  :clock-in t :clock-resume t :empty-lines 1)

(akirak/def-org-reverse-datetree-refile "journal.org"
  :key "j"
  :ask-always t :prefer '("SCHEDULED" "CLOSED" "CREATED_TIME" "CREATED_AT"))

(defun akirak/org-schedule-journal-entry (date)
  (interactive (list (org-read-date nil nil nil "Scheduled date: ")))
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in org-mode"))
  (org-schedule nil date)
  (org-reverse-datetree-refile-to-file
   (org-starter-locate-file "journal.org" nil t) date))

;;;;;; code.org
(org-starter-def-capture "co" "code.org: Stub entry (with %i as body)"
  entry (file+function "code.org" org-reverse-datetree-goto-read-date-in-file)
  "* %^{Title}
:PROPERTIES:
:CREATED_TIME: %U
:working_on: %k
:END:

%(unless (string-empty-p \"%i\") \"%i\n\n\")%?"
  :clock-in t :clock-resume t :empty-lines 1)

(akirak/def-org-reverse-datetree-refile "code.org"
  :key "co"
  :ask-always arg :prefer '("CLOSED" "CREATED_TIME" "CREATED_AT"))

;;;; Resources
;;;;; org-capture
;;- [[http://www.howardism.org/Technical/Emacs/capturing-content.html][Capturing Content for Emacs]]
;;- [[https://github.com/alphapapa/org-web-tools][alphapapa/org-web-tools]]
;;- [[https://github.com/alphapapa/org-protocol-capture-html][alphapapa/org-protocol-capture-html]]
;;- [[https://www.reddit.com/r/emacs/comments/9ycgoe/do/][Do you guys have a personal wiki? (using org-mode or not) : emacs]]
;;  - [[https://www.reddit.com/r/emacs/comments/9ycgoe/do_you_guys_have_a_personal_wiki_using_orgmode_or/ea0j8nw/][Example configuration by alphapapa]]
;;- [[https://github.com/abo-abo/orca][GitHub - abo-abo/orca: ORg CApture]]

(provide 'setup-org-starter)
