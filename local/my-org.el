(setq org-starter-require-file-by-default nil)

;;;; Org directories

(org-starter-def "~/org/"
  :add-to-path t
  :custom-vars (org-directory)
  (org-starter-def "~/org/journal"
    :custom-vars org-journal-dir
    :config
    (require 'init-org-journal)))

(org-starter-def "~/learning"
  :add-to-path t)

(org-starter-def "~/learning/natural-languages"
  :id language-learning
  :add-to-path t
  :agenda nil
  :refile (:maxlevel . 1)
  :files
  ("english.org")
  ("chinese.org")
  ("japanese.org"))

(org-starter-def "~/private/ledger"
  :add-to-path t)

;;;; Org files

(org-starter-define-file "scratch.org"
  :key "i"
  :required nil
  :custom-vars 'org-default-notes-file
  :agenda t
  :refile '(:maxlevel . 2))

(org-starter-define-file "tasks.org"
  :required nil
  :agenda t
  :refile '(:maxlevel . 9))

(org-starter-define-file "planner.org"
  :key "p"
  :agenda t
  :required nil
  :refile '(:maxlevel . 3)
  :custom-vars '(org-focus-file)
  :capture `(("gr" "Item to read" entry
              (lambda ()
                (goto-char (org-find-property "CUSTOM_ID" "main-reading-list")))
              ,(akirak/org-capture-entry-template-1
                   "^{Title}"
                 "%i"
                 :todo "TODO"))))

(org-starter-define-file "subjects.org"
  :key "s"
  :agenda t
  :refile '(:maxlevel . 9))

(org-starter-define-file "brainstorming.org"
  :key "b"
  :agenda t
  :refile '(:maxlevel . 9)
  :capture
  '(("R" "Reflection" plain
     (file+function
      (lambda ()
        (goto-char (org-find-property "CUSTOM_ID" "reflection"))
        (end-of-line)
        (re-search-forward "^\*" nil t)
        (end-of-line 0)
        (insert "\n")))
     "%i %U"
     :empty-lines 1)))

(org-starter-define-file "workflow.org"
  :key "w"
  :agenda nil
  :refile '(:maxlevel . 4))

(org-starter-define-file "icebox.org"
  :key "m"
  :refile '(:maxlevel . 9))

(defun akirak/buffer-mode-name (filename)
  (with-current-buffer filename
    (string-remove-suffix "-mode" (symbol-name major-mode))))

(org-starter-define-file "code.org"
  :key "c"
  :required nil
  :agenda t
  :refile nil
  ;; Add templates for specific languages.
  :capture `(("c" "Code (org-babel)")
             ("cc" "Capture code" entry
              (file+function akirak/org-reverse-date-tree)
              ,(akirak/org-capture-entry-template-1 "%?%(which-function)"
                 "%a\n\n#+BEGIN_SRC %(akirak/buffer-mode-name \"%F\")\n%i\n#+END_SRC\n"
                 :todo "TODO")
              :clock-in t :clock-resume t)
             ("ce" "Babel code, Emacs Lisp" entry
              (file+function akirak/org-reverse-date-tree)
              ,(akirak/babel-capture-template "emacs-lisp")
              :clock-in t :clock-resume t)))

(org-starter-define-file "posts.org"
  :key "P"
  :required nil
  :agenda t
  :refile '(:level . 1))

(org-starter-define-file "accounting.org"
  :key "M"
  :agenda t
  :refile '(:maxlevel . 3))

(org-starter-define-file "yankpad.org"
  :set-default 'yankpad-file
  :refile '(:level . 1))

;;;; org-agenda custom commands (currently unused)

;; (akirak/org-add-agenda-custom-command "r" "Reading"
;;   '((tags "CATEGORY=\"readings\""
;;           ((org-agenda-prefix-format "  ")
;;            (org-tags-match-list-sublevels nil)
;;            (org-agenda-sorting-strategy '(deadline-down priority-up))
;;            (org-super-agenda-groups
;;             '((:todo "REVIEW"))))))
;;   nil
;;   (list (akirak/org-agenda-view-file "reading.html")))

;;;; Custom rifle commands

(defun akirak/helm-org-rifle-knowledge-base ()
  (interactive)
  (helm-org-rifle-files (delq nil
                              (mapcar (lambda (fname)
                                        (org-starter-locate-file fname nil t))
                                      '("scratch.org"
                                        "workflow.org"
                                        "emacs.org"
                                        "code.org"
                                        "posts.org"
                                        "brainstorming.org"
                                        "subjects.org"
                                        "planner.org")))))

(general-def :prefix "<menu>"
  "<menu>" #'akirak/helm-org-rifle-knowledge-base)

;;;; Other org options

(with-eval-after-load 'org-clock
  (org-clock-persistence-insinuate))

;;;; frame-workflow

(akirak/define-frame-workflow "org"
  :key "o"
  :layout '(progn
             (org-starter-load-all-known-files)
             (when (fboundp #'ibuffer-sidebar-show-sidebar)
               (ibuffer-sidebar-show-sidebar)))
  :make-frame '(frame-purpose-make-mode-frame 'org-mode))

(provide 'my-org)
