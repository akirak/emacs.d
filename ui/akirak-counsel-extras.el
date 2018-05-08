;;;; counsel-linux-app
(defun akirak/counsel-linux-app-format-function (name comment command)
  (format "%-25s: %s" name command))

(defun akirak/counsel-linux-apps-sort-list (alist)
  (sort alist (lambda (x y)
                (< (length (cdr x)) (length (cdr y))))))

(with-eval-after-load 'counsel
  (advice-add 'counsel-linux-apps-parse
              :filter-return 'akirak/counsel-linux-apps-sort-list)

  (ivy-add-actions 'counsel-linux-app
                   '(("j" (lambda (cand)
                            (split-window-sensibly)
                            (other-window 1)
                            (counsel-linux-app-action-default cand))
                      "other window"))))

(setq counsel-linux-app-format-function 'akirak/counsel-linux-app-format-function)

;;;; counsel-external-command
(defvar akirak/executable-list nil)

(defun akirak/get-executable-list (&optional sort)
  (or akirak/executable-list
      (setq akirak/executable-list
            (cl-loop for dir in exec-path
                     when (and (file-exists-p dir) (file-accessible-directory-p dir))
                     for lsdir = (cl-loop for i in (directory-files dir t)
                                          for bn = (file-name-nondirectory i)
                                          when (and (not (member bn completions))
                                                    (not (file-directory-p i))
                                                    (file-executable-p i))
                                          collect bn)
                     append lsdir into completions
                     finally return
                     (if sort (sort completions 'string-lessp) completions)))))

(defun akirak/counsel-external-command ()
  "Run a custom command."
  (interactive)
  (let ((ivy-format-function (lambda (cands)
                               (ivy--format-function-generic
                                (lambda (str)
                                  (ivy--add-face str 'ivy-current-match))
                                #'identity
                                cands
                                " "))))
    (ivy-read "Command: " (akirak/get-executable-list)
              :caller 'akirak/counsel-external-command
              :action (lambda (cand)
                        (start-process-shell-command cand nil cand)))))

(provide 'akirak-counsel-extras)
