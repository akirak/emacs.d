;;; init-repom.el --- Configuration for the repository manager -*- lexical-binding: t -*-

(use-package helm-projectile)

(use-package repom
  :straight (repom :host github :repo "akirak/repom.el")
  :commands (helm-repom))

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

(provide 'init-repom)
;;; init-repom.el ends here
