(setq initial-buffer-choice
      (when (and (not initial-buffer-choice)
                 init-file-debug)
        (lambda () (get-buffer "*Messages*"))))

;; Prevent from initializing lisp-interaction-mode
(setq initial-major-mode 'text-mode
      initial-scratch-message "")

(use-package org-status
  :disabled t
  :straight (org-status :host github :repo "akirak/org-status")
  :config
  (unless init-file-debug
    (org-status-initial-buffer-mode 1))
  :custom
  (org-status-default-directory user-emacs-directory)
  (org-status-header #'org-status-chemacs-header))

(defun org-status-chemacs-header ()
  (let ((args `(("emacs-version" . ,(emacs-version))

                ("uname" . ,(string-trim-right
                             (shell-command-to-string "uname -a")))
                ("startup" . ,(format-time-string "%F %R" after-init-time))
                ("after-init-time" . ,(float-time (time-subtract after-init-time
                                                                 before-init-time)))
                ("args-info" . ,(if (cdr command-line-args)
                                    (concat "\nwith arguments "
                                            (prin1-to-string (cdr command-line-args)))
                                  ""))
                ("user-emacs-directory" . ,(expand-file-name user-emacs-directory))
                ("custom-file-info" . ,(if custom-file
                                           (format "[[file:%s][%s]]"
                                                   custom-file
                                                   (expand-file-name custom-file))
                                         "nil"))
                ("chemacs-info" . ,(if chemacs-emacs-profiles
                                       (format "Chemacs was activated with =%s= profile ([[file:%s][edit profiles]])"
                                               chemacs-current-emacs-profile
                                               chemacs-profiles-path)
                                     "Chemacs was not activated")))))
    (s-format
     "#+title: Status
- ${emacs-version}
- ${uname}

Emacs started at ${startup} (initialised in ${after-init-time}s)${args-info}
${chemacs-info}
- =user-emacs-directory= :: [[file:${user-emacs-directory}][${user-emacs-directory}]]
- =custom-file= :: ${custom-file-info}"
     'aget args)))

(provide 'setup-initial-buffer)
