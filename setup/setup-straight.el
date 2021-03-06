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
    (unless (null packages)
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
               total (float-time (time-subtract finish-time start-time))))))

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
                 '(("u" akirak/straight-use-package "Install")
                   ("r" akirak/find-readme-or-browse-emacs-package "Readme")
                   ("b" akirak/browse-emacs-package "Browse source repo")
                   ("m" akirak/browse-emacs-package-in-registry "Browse in MELPA")
                   ("lo" akirak/store-emacs-package-repository-link "Store org link")
                   ("lw" akirak/copy-emacs-package-repository-url "Copy repository URL")
                   ("C" akirak/git-clone-emacs-package "Clone")))

(defun akirak/straight-use-package (name)
  "A variant of `straight-use-package' that accepts a package name."
  (straight-use-package (intern name)))

(defun akirak/straight-rebuild-package (package &optional recursive)
  (interactive
   (list (let ((packages (let ((packages nil))
                           (maphash (lambda (package recipe)
                                      (unless (or (plist-get recipe :no-build)
                                                  (or (null (plist-get recipe :local-repo))
                                                      (not (straight--repository-is-available-p
                                                            recipe))))
                                        (push package packages)))
                                    straight--recipe-cache)
                           packages))
               (dirname (f-filename default-directory))
               (basename (and (eq major-mode 'emacs-lisp-mode)
                              (file-name-base (buffer-file-name)))))
           (completing-read "Rebuild package: "
                            packages
                            (lambda (_) t)
                            'require-match
                            nil nil
                            (or (and (member basename packages) basename)
                                (and (member dirname packages) dirname))))
         current-prefix-arg))
  (straight-rebuild-package package)
  (when (yes-or-no-p "Reload the package? ")
    (load-library package)))

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

(defun akirak/emacs-package-repository-html-url (package)
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
       (url (plist-get plist :url))
       (type (plist-get plist :type)))
    (cl-case host
      (github
       (concat "https://github.com/"
               repo (if (or commit branch)
                        (concat "/tree/" (or commit branch))
                      "")))
      (gitlab
       (concat "https://gitlab.com/"
               repo (if (or commit branch)
                        (concat "/tree/" (or commit branch))
                      "")))
      (otherwise
       (cond
        (url `(url ,url))
        ((and (eq type 'git) repo) repo)
        (t `(recipe ,recipe)))))))

(defun akirak/git-clone-emacs-package (package)
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
       (akirak/git-clone (format "https://github.com/%s.git" repo)
                         (when branch
                           (list "-b" branch))))
      (gitlab
       (akirak/git-clone (format "https://gitlab.com/%s.git" repo)
                         (when branch
                           (list "-b" branch))))
      (otherwise
       (cond
        (url (akirak/git-clone url (when branch (list "-b" branch))))
        (t (error "Cannot get the repository URL for %s" recipe)))))))

(defun akirak/browse-emacs-package (package)
  (interactive (list (completing-read "Emacs package: "
                                      #'akirak/emacs-package-list)))
  (pcase (akirak/emacs-package-repository-html-url package)
    (`(url ,url)
     (browse-url url))
    (`(recipe ,recipe)
     (progn
       (kill-new (prin1-to-string recipe))
       (message "Copied %s to the kill ring" recipe)))
    (url
     (browse-url url))))

(defun akirak/browse-emacs-package-in-registry (package)
  (pcase-let*
      ((recipe (akirak/get-emacs-package-recipe
                (cl-etypecase package
                  (string (intern package))
                  (symbol package))))
       (plist (if (string-prefix-p ":" (symbol-name (car recipe)))
                  recipe
                (cdr recipe)))
       (flavour (plist-get plist :flavor)))
    (cl-ecase flavour
      (melpa
       (browse-url (format "https://melpa.org/#/%s" package))))))

(defun akirak/copy-emacs-package-repository-url (package)
  (pcase (akirak/emacs-package-repository-html-url package)
    (`(url ,url)
     (progn
       (kill-new url)
       (message "Copied the Git repository URL instead" url)))
    (`(recipe ,recipe)
     (progn
       (kill-new (prin1-to-string recipe))
       (message "Copied %s to the kill ring" recipe)))
    (url
     (kill-new url))))

(defun akirak/store-emacs-package-repository-link (package)
  (require 'ol)
  (let ((url (pcase (akirak/emacs-package-repository-html-url package)
               (`(url ,url)
                url)
               (`(recipe ,recipe)
                (user-error "URL not available from recipe %s" recipe))
               (url
                url))))
    (push (list url package) org-stored-links)))

(defun akirak/straight-browse-source (package)
  (interactive
   (list (straight--select-package "Fetch and browse: " nil 'installed)))
  (straight-fetch-package package 'upstream)
  (magit-status (file-name-directory (file-truename (find-library-name package)))))

(akirak/bind-admin
  "el" 'akirak/ivy-emacs-package
  "ew" 'akirak/browse-emacs-package
  "eb" 'akirak/straight-rebuild-package
  "eB" '(akirak/straight-rebuild-outdated-packages :wk "Rebuild outdated")
  "eF" #'straight-pull-package
  "ef" 'akirak/straight-browse-source)

(provide 'setup-straight)
