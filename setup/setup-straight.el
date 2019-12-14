;; Extra functions for straight.el

(defvar akirak/straight-rebuilt-outdated-packages nil)

(defun akirak/straight-get-outdated-packages ()
  (with-current-buffer "*Messages*"
    (let (result)
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward (rx bol "Source file ‘"
                                      (group (+? anything))
                                      "’ newer than byte-compiled file" eol)
                                  nil t)
          (let* ((fpath (buffer-substring-no-properties
                         (match-beginning 1)
                         (match-end 1)))
                 (package (when (string-match (rx (group (+ (not (any "/"))))
                                                  "/" (+ (not (any "/"))) eol)
                                              fpath)
                            (match-string 1 fpath))))
            (push package result))))
      (set-difference (cl-remove-duplicates (nreverse result)
                                            :test #'string-equal)
                      akirak/straight-rebuilt-outdated-packages))))

(defun akirak/straight-rebuild-outdated-packages (&optional reload)
  "Rebuild outdated packages."
  (interactive "P")
  (let* ((packages (akirak/straight-get-outdated-packages))
         (total (length packages))
         (i 1)
         (start-time (current-time))
         finish-time
         package)
    (while (setq package (pop packages))
      (message "Rebuilding packages %s (%d/%d)..."
               package i total)
      (let (message-log-max)
        (straight-rebuild-package package))
      (push package akirak/straight-rebuilt-outdated-packages)
      (when reload
        (load package))
      (setq i (1+ i)))
    (setq finish-time (current-time))
    (garbage-collect)
    (message "Finished rebuilding %d package in %.1f seconds."
             total (float-time (time-subtract finish-time start-time)))))

;; After an idle of 10 minutes, rebuild the outdated packages.
(run-with-idle-timer 600 nil #'akirak/straight-rebuild-outdated-packages)

(defun akirak/emacs-package-list (&rest _args)
  (cl-remove-if-not (lambda (name) (string-match-p (rx bol alnum) name))
                    (straight-recipes-list straight-recipe-repositories)))

(memoize #'akirak/emacs-package-list)

(defun akirak/get-emacs-package-recipe (name)
  (straight-recipes-retrieve name straight-recipe-repositories))

(cl-defun akirak/ivy-emacs-package (&key action caller)
  (interactive)
  (when current-prefix-arg
    (straight-pull-package "melpa")
    ;; Clean the cache
    (memoize-restore #'akirak/emacs-package-list)
    (memoize #'akirak/emacs-package-list))
  (ivy-read (format "Emacs package [%s]: "
                    (mapconcat #'symbol-name straight-recipe-repositories " "))
            #'akirak/emacs-package-list
            :caller (or caller 'akirak/ivy-emacs-package)
            :action
            (lambda (inp)
              (let ((package (intern inp)))
                (funcall (or action #'akirak/find-readme-or-browse-emacs-package) package)))
            :require-match t))

(ivy-add-actions 'akirak/ivy-emacs-package
                 '(("u" straight-use-package "Install")
                   ("r" akirak/find-readme-or-browse-emacs-package "Readme")
                   ("b" akirak/browse-emacs-package "Browse source repo")))

(defun akirak/find-readme-or-browse-emacs-package (package)
  (let* ((recipe (akirak/get-emacs-package-recipe
                  (cl-etypecase package
                    (string (intern package))
                    (symbol package))))
         (plist (if (string-prefix-p ":" (symbol-name (car recipe)))
                    recipe
                  (cdr recipe))))
    (cl-case (plist-get plist :host)
      ('github
       (akirak/browse-github-readme (plist-get plist :repo)))
      ('gitlab
       (princ package))
      (otherwise
       (user-error "Unsupported recipe %s" recipe)))))

(defun akirak/browse-emacs-package (package)
  (pcase-let*
      ((recipe (akirak/get-emacs-package-recipe
                (cl-etypecase package
                  (string (intern package))
                  (symbol package))))
       (plist (if (string-prefix-p ":" (symbol-name (car recipe)))
                  recipe
                (cdr recipe)))
       (host (plist-get plist :host))
       (repo (plist-get plist :repo))
       (branch (plist-get plist :branch))
       (commit (plist-get plist :commit))
       (url (plist-get plist :url)))
    (cl-case host
      (github
       (browse-url (concat "https://github.com/"
                           repo (if (or commit branch)
                                    (concat "/tree/" (or commit branch))
                                  ""))))
      (gitlab
       (browse-url (concat "https://gitlab.com/"
                           repo (if (or commit branch)
                                    (concat "/tree/" (or commit branch))
                                  ""))))
      (otherwise
       (cond
        (url (progn
               (kill-new url)
               (message "Saved %s to the kill ring" url)))
        (t (progn
             (kill-new (prin1-to-string recipe))
             (message "Copied %s to the kill ring" recipe))))))))

(pretty-hydra-define akirak/package-hydra
  (:title "Packages"
          :exit t)
  ("Straight.el"
   (("el" akirak/ivy-emacs-package "List all")
    ("e!" akirak/straight-rebuild-outdated-packages "Rbld outdtd"))))

(general-def "C-x M-p" #'akirak/package-hydra/body)

(provide 'setup-straight)
