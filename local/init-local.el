;;; init-local.el --- Root of my personal configuration -*- lexical-binding: t -*-

(require 'init-org-starter)
(require 'init-org-capture)
(require 'init-org-agenda)

;;;; Optional repositories

(cl-defmacro akirak/load-config-from-directory (dir
                                                &key
                                                symbol
                                                org-dir-options
                                                org-files)
  "Load init.el in a directory.

This macro does the following things in this order:

1. If DIR exists and SYMBOL is non-nil, set SYMBOL to DIR.

2. If init.el exists in DIR, load the file.

Optionally, you can define the directory as an org directory as well
as files in the directory using org-starter. Options for the directory
can be defined in ORG-DIR-OPTIONS, and files can be defined in
ORG-FILES."
  (declare (indent 1))
  (let ((local-init-file (expand-file-name "init.el" dir)))
    `(when (file-directory-p ,dir)
       ,(when symbol
          `(setq ,symbol ,dir))
       ;; This should be run before load-file, as the local-init-file
       ;; can contain org-starter-define-file statements that depends
       ;; on the directory configuration, e.g. load-path.
       (org-starter-define-directory ,dir
         ,@org-dir-options
         :files (quote ,org-files))
       (when (file-exists-p ,local-init-file)
         (condition-case err
             (load-file ,local-init-file)
           (error (message "Error while loading %s: %s"
                           ,local-init-file err)))))))

;; These Git repositories are deployed by Ansible

(akirak/load-config-from-directory "~/ops"
  :org-dir-options (:agenda t))

;; Deprecated
(akirak/load-config-from-directory "~/org"
  :org-dir-options (:add-to-path t)
  :symbol org-directory)

(akirak/load-config-from-directory "~/learning"
  :org-dir-options (:add-to-path t))

(akirak/load-config-from-directory "~/hugo"
  :symbol hugo-project-directory)

(akirak/load-config-from-directory "~/private"
  :org-dir-options (:add-to-path t))

(akirak/load-config-from-directory "~/personal/ledger"
  :org-dir-options (:add-to-path t))

(akirak/load-config-from-directory "~/personal/org-journal"
  :symbol org-journal-dir)

(akirak/load-config-from-directory "~/personal/language-learning"
  :org-dir-options (:id 'language-learning
                        :add-to-path t
                        :agenda nil
                        :refile '(:maxlevel . 1)))

(provide 'init-local)
;;; init-local.el ends here
