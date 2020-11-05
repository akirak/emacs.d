;;; -*- mode: emacs-lisp; lexical-binding: t; -*-

(defcustom akirak/git-clone-directory "~/projects/"
  "FIXME")

(defcustom akirak/git-clone-parent-alist
  `(("github.com" . ,(expand-file-name "github" akirak/git-clone-directory)))
  "FIXME"
  :type '(alist :key-type string :value-type file))

(defun akirak/git-clone-parent-for-host (host)
  (or (cdr (assoc host akirak/git-clone-parent-alist))
      (expand-file-name host akirak/git-clone-directory)))

(defcustom akirak/github-login "akirak"
  "The user name on GitHub.")

(defun akirak/remote-git-repo-clone-1 (repo)
  (let* ((url (akirak/remote-git-repo-url repo))
         (branch (akirak/remote-git-repo-branch repo))
         (branch (if (member branch '("master" "main"))
                     nil
                   branch))
         (dest (f-join (akirak/remote-git-repo-clone-parent repo)
                       (concat (akirak/remote-git-repo-name repo)
                               (if branch
                                   (concat "@" branch)
                                 ""))))
         (args (transient-args 'magit-clone)))
    (if (file-directory-p dest)
        (progn
          (message "Already exists: %s" dest)
          (magit-status dest))
      (magit-clone-regular url dest `(,(and branch `("-b" ,branch))
                                      ,@args)))))

(cl-defgeneric akirak/remote-git-repo-clone-default (repo)
  (akirak/remote-git-repo-clone-1 repo))

(cl-defgeneric akirak/remote-git-repo-url-github-p (_)
  nil)

(cl-defgeneric akirak/remote-git-repo-url (repo))
(cl-defgeneric akirak/remote-git-repo-owner (repo))
(cl-defgeneric akirak/remote-git-repo-name (repo))
(cl-defgeneric akirak/remote-git-repo-clone-parent (repo))
(cl-defgeneric akirak/remote-git-repo-branch (_)
  nil)
(cl-defgeneric akirak/remote-git-repo-file (_)
  nil)

;;;; GitHub repositories (https)

(cl-defstruct akirak/github-https-repo owner name branch file)
(cl-defmethod akirak/remote-git-repo-url ((repo akirak/github-https-repo))
  (concat "https://github.com/"
          (akirak/github-https-repo-owner repo)
          "/"
          (akirak/github-https-repo-name repo)
          ".git"))
(cl-defmethod akirak/remote-git-repo-owner ((repo akirak/github-https-repo))
  (akirak/github-https-repo-owner repo))
(cl-defmethod akirak/remote-git-repo-name ((repo akirak/github-https-repo))
  (akirak/github-https-repo-name repo))
(cl-defmethod akirak/remote-git-repo-clone-parent ((repo akirak/github-https-repo))
  (akirak/git-clone-parent-for-host "github.com"))
(cl-defmethod akirak/remote-git-repo-url-github-p ((repo akirak/github-https-repo))
  t)
(cl-defmethod akirak/remote-git-repo-branch ((repo akirak/github-https-repo))
  (akirak/github-https-repo-branch repo))
(cl-defmethod akirak/remote-git-repo-file ((repo akirak/github-https-repo))
  (akirak/github-https-repo-file repo))

;;;; GitHub repositories (ssh)

(cl-defstruct akirak/github-ssh-repo owner name branch file)
(cl-defmethod akirak/remote-git-repo-url ((repo akirak/github-ssh-repo))
  (concat "git@github.com:"
          (akirak/github-ssh-repo-owner repo)
          "/"
          (akirak/github-ssh-repo-name repo)
          ".git"))
(cl-defmethod akirak/remote-git-repo-owner ((repo akirak/github-ssh-repo))
  (akirak/github-ssh-repo-owner repo))
(cl-defmethod akirak/remote-git-repo-name ((repo akirak/github-ssh-repo))
  (akirak/github-ssh-repo-name repo))
(cl-defmethod akirak/remote-git-repo-clone-parent ((repo akirak/github-ssh-repo))
  (akirak/git-clone-parent-for-host "github.com"))
(cl-defmethod akirak/remote-git-repo-url-github-p ((repo akirak/github-ssh-repo))
  t)

;;;; Generic Git repositories

(cl-defstruct akirak/generic-git-repo url protocol host owner name)
(cl-defmethod akirak/remote-git-repo-url ((repo akirak/generic-git-repo))
  (or (akirak/generic-git-repo-url repo)
      (pcase (akirak/generic-git-repo-protocol repo)
        ("ssh"
         (concat "git@"
                 (akirak/generic-git-repo-host repo) ":"
                 (akirak/generic-git-repo-owner repo) "/"
                 (akirak/generic-git-repo-name repo)
                 ".git"))
        ("https"
         (concat "https://"
                 (akirak/generic-git-repo-host repo) "/"
                 (akirak/generic-git-repo-owner repo) "/"
                 (akirak/generic-git-repo-name repo)
                 ".git")))))
(cl-defmethod akirak/remote-git-repo-name ((repo akirak/generic-git-repo))
  (akirak/generic-git-repo-name repo))
(cl-defmethod akirak/remote-git-repo-clone-parent ((repo akirak/generic-git-repo))
  ;; TODO: Look up
  "~/tmp/")

;;;; Parsing

(defun akirak/parse-git-url (string)
  (let* ((name-pattern (rx bol (group (+? (any alnum "-_."))) eol))
         (path-pattern (rx (group (+ (any alnum "-")))
                           "/"
                           (group (+? (any alnum "-_.")))))
         (host-pattern (rx (group (+ (not (any "./")))
                                  (+ (and "." (+ (not (any "./"))))))))
         (suffix (rx ".git"))
         (optional-suffix (rx (?  ".git")))
         (github-path-pattern (concat "^" path-pattern "$"))
         (https-pattern (concat "^https://" host-pattern "/" path-pattern
                                (rx (? (or (and "/blob/" (group (+ (any alnum "-")))
                                                "/" (group (and (any alnum ".")
                                                                (+ anything))))
                                           ".git"
                                           "/"))
                                    eol)))
         (ssh-pattern (concat "^git@" host-pattern ":" path-pattern (rx ".git" eol))))
    (pcase-let* ((`(,protocol ,url ,host ,owner ,name . ,rest)
                  (or (-some->> (s-match name-pattern string)
                        (append (list nil nil "github.com" akirak/github-login)))
                      (-some->> (s-match github-path-pattern string)
                        (cdr)
                        (append (list nil nil "github.com")))
                      (-some->> (s-match https-pattern string)
                        (cons 'https))
                      (-some->> (s-match ssh-pattern string)
                        (cons 'ssh))))
                 (`(,branch ,file) rest))
      (cond
       ((and (equal host "github.com") (equal owner akirak/github-login))
        (make-akirak/github-ssh-repo :owner owner
                                     :name (string-remove-suffix ".git" name)))
       ((equal host "github.com")
        (make-akirak/github-https-repo :owner owner
                                       :name (string-remove-suffix ".git" name)
                                       :branch branch
                                       :file file))
       ((and host owner name)
        (make-akirak/generic-git-repo :url string
                                      :protocol protocol
                                      :host host
                                      :owner owner
                                      :name (string-remove-suffix ".git" name)))
       (t
        (make-akirak/generic-git-repo :url string))))))

;;;; Commands

(defun akirak/git-clone-remote-repo (path-or-url)
  (interactive "sGit repository (path or url): ")
  (akirak/remote-git-repo-clone-default (akirak/parse-git-url path-or-url)))

;;;; Configuration for Magit

(advice-add #'magit-worktree-checkout
            :after
            (defun akirak/maybe-fork-github-repo (&rest _args)
              (let* ((origin (car (magit-config-get-from-cached-list "remote.origin.url")))
                     (repo (if origin
                               (akirak/parse-git-url origin)
                             (user-error "No origin is set"))))
                (when (and (akirak/github-https-repo-p repo)
                           (not (equal (akirak/github-https-repo-owner repo)
                                       akirak/github-login))
                           (when (member akirak/github-login (magit-list-remotes))
                             (message "Remote %s already exists" akirak/github-login)
                             nil))
                  (forge-fork akirak/github-login akirak/github-login)))))


(setq magit-worktree-read-directory-name-function
      (defun akirak/read-git-worktree-name (prompt)
        (if-let* ((origin (car (magit-config-get-from-cached-list "remote.origin.url")))
                  (repo (and origin
                             (akirak/parse-git-url origin)))
                  (parent (akirak/remote-git-repo-clone-parent repo)))
            (read-directory-name prompt
                                 parent
                                 (akirak/remote-git-repo-name repo))
          (read-directory-name prompt))))

;;;; Commonplace submodules

(defcustom commonplace-root nil
  "Root of the commonplace repo."
  :type 'directory)

(defcustom commonplace-repos-clone-root nil
  "Root directory inside which submodule are added."
  :type 'directory)

(defcustom commonplace-repos-find-root nil
  "Root directory in which Emacs actually visits for files."
  :type 'directory)

(defun commonplace-repos--all-submodule-urls ()
  (let ((file (f-join commonplace-root ".gitmodules")))
    (when (f-exists-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (let (result)
          (while (re-search-forward (rx bol (+ space) "url" (* space) "="
                                        (* space))
                                    nil t)
            (push (buffer-substring-no-properties (point) (line-end-position))
                  result))
          (nreverse result))))))

(defun commonplace-repos--active-submodule-urls ()
  (let ((default-directory commonplace-root))
    (->> (process-lines "git" "config" "--list" "--local")
         (-map (lambda (s)
                 (nth 1 (s-match (rx bol "submodule." (+ anything)
                                     ".url=" (group (+ anything)) eol)
                                 s))))
         (delq nil))))

(defun commonplace-repos--has-submodule-p (path)
  (member path (commonplace-repos--all-submodule-urls)))

(cl-defun commonplace-find-repo-module (path-or-url
                                        &key no-visit)
  (interactive (list
                (let* ((active-submodules (commonplace-repos--active-submodule-urls))
                       (all-submodules (-map (lambda (path)
                                               (if (member path active-submodules)
                                                   (propertize path
                                                               'face 'font-lock-string-face)
                                                 path))
                                             (commonplace-repos--all-submodule-urls))))
                  ;; TODO: Set the next history element to
                  ;; (magit-config-get-from-cached-list "remote.origin.url")
                  (completing-read "Add or visit submodule: "
                                   all-submodules
                                   nil nil nil nil))))
  (let* ((obj (akirak/parse-git-url path-or-url))
         (host (cond
                ((or (akirak/github-https-repo-p obj)
                     (akirak/github-ssh-repo-p obj))
                 "github.com")
                ((akirak/generic-git-repo-p obj)
                 (akirak/generic-git-repo-host obj))))
         (owner (akirak/remote-git-repo-owner obj))
         (name (akirak/remote-git-repo-name obj))
         (obj (cond
               ;; Forbidden adding a private Git repository
               ;; ((and (akirak/generic-git-repo-p obj)
               ;;       (equal "ssh" (akirak/generic-git-repo-protocol obj)))
               ;;  (setf (akirak/generic-git-repo-protocol obj) "https")
               ;;  (setf (akirak/generic-git-repo-url obj) nil))
               ((akirak/github-ssh-repo-p obj)
                (make-akirak/github-https-repo :owner owner
                                               :name name))
               (t obj)))
         ;; TODO: Add support for generic URLs without owner ane name
         (path-with-host (progn
                           (cl-assert host)
                           (cl-assert owner)
                           (cl-assert name)
                           (f-join host owner name)))
         (default-directory commonplace-root)
         (submodule-rel-path (progn
                               (cl-assert commonplace-root)
                               (cl-assert commonplace-repos-clone-root)
                               (f-join (f-relative commonplace-repos-clone-root
                                                   commonplace-root)
                                       path-with-host)))
         (submodule-abs-path (f-join commonplace-root submodule-rel-path))
         (url (akirak/remote-git-repo-url obj)))
    (cl-labels
        ((visit
          ()
          (unless no-visit
            (dired (f-join commonplace-repos-find-root
                           path-with-host))))
         (update-process-sentinel
          (process event)
          (when (eq (process-status process) 'exit)
            (if (= (process-exit-status process) 0)
                (visit)
              (error "Error while updating a submodule: %s"
                     event))))
         (add-process-sentinel
          (process event)
          (when (eq (process-status process) 'exit)
            (if (= (process-exit-status process) 0)
                (progn
                  (message (call-process-with-args "systemctl"
                             "--user" "restart" "commonplace-repos.service"))
                  (visit))
              (error "Error while cloning a submodule: %s"
                     event)))))
      (cond
       ((f-directory-p submodule-abs-path)
        (message "%s already exists" submodule-rel-path)
        (visit))
       ;; The submodule exists, but not inited
       ((member url (commonplace-repos--all-submodule-urls))
        (message "Initializing an existing submodule for %s" url)
        (magit-with-toplevel
          (make-process :name "git-submodule-update"
                        :command (list "git"
                                       "submodule" "update"
                                       "--init"
                                       "--depth" "1"
                                       "--"
                                       submodule-rel-path)
                        :sentinel #'update-process-sentinel)))
       (t
        ;; I won't use `magit-submodule-add-1' because I want to
        ;; alternate the process sentinel.
        (message "Cloning %s as a submodule" url)
        (magit-with-toplevel
          (make-process :name "git-submodule-add"
                        :command (list "git"
                                       "submodule" "add"
                                       "--depth" "1"
                                       "--"
                                       url
                                       submodule-rel-path)
                        :sentinel #'add-process-sentinel)))))))

(defun commonplace-repos-counsel-rg ()
  (interactive)
  (counsel-rg nil commonplace-repos-find-root))

(provide 'setup-git-clone)
