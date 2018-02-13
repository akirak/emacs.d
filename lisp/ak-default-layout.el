;;; ak-default-layout.el --- The default layout for my Emacs configuration

;; Copyright (C) 2018 by Akira Komamura

;;; Commentary:

;; This is a fallback default layout. If ~/.emacs.d/private/init.el does not
;; exist, this file is loaded as an alternative after other configuration tasks
;; defined in ~/.emacs.d/init.el. 

;;; Code:

;;;; Fallback personalization

;; This configuration is loaded if and only if `akirak/personalized' variable is
;; non-nil. 
(when akirak/personalized
  ;; Fallback configuration for org-mode
  (setq org-default-notes-file (if (file-directory-p "~/Dropbox")
                                   "~/Dropbox/org/todo/Inbox.org"
                                 "~/Inbox.org"))
  (let ((task-template '("t" "Task" entry (file+olp org-default-notes-file "Tasks")
                         "* TODO %?\n:PROPERTIES:\n:CREATED_AT: %U\n:END:\n\n\n"
                         :prepend t
                         :clock-in t :clock-resume t)))
    (with-eval-after-load 'org
      (push task-template org-capture-templates))))

(provide 'ak-default-layout)

;;; ak-default-layout.el ends here
