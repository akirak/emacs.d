;;; init-repom.el --- Configuration for the repository manager -*- lexical-binding: t -*-

(use-package helm-projectile
  :after (helm projectile))

(use-package repom
  :straight (repom :host github :repo "akirak/repom.el"
                   :files (:defaults
                           (:exclude "helm-repom.el")))
  :custom
  (repom-local-discovery-locations
   '(("~/github/" 1 :name "github")
     ("~/docs/" 1 :name "docs")
     ("~/.emacs.d/straight/repos/" 1 :name "straight")
     ("~/personal/" 1 :name "personal")
     ("~/" 1 :pattern "^[^\.]"))))

(when (version< "26" emacs-version)
  (defun akirak/repom-async-get-lists ()
    (make-thread
     (lambda ()
       (message "Retrieving lists of GitHub repositories...")
       (require 'repom-github)
       (repom-github-fetch-repo-lists)
       (message "Finished retrieving repository lists."))))

  (add-hook 'emacs-startup-hook
            (lambda ()
              (run-with-timer 2 nil #'akirak/repom-async-get-lists))))

(use-package ghub)

(use-package helm-repom
  :straight (helm-repom :host github :repo "akirak/repom.el"
                        :files ("helm-repom.el"))
  :commands (helm-repom))

(provide 'init-repom)
;;; init-repom.el ends here
