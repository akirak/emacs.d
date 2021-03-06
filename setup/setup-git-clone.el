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
         (dest (akirak/git-default-clone-destination repo :create-parent t))
         (args (transient-args 'magit-clone)))
    (if (file-directory-p dest)
        (progn
          (message "Already exists: %s" dest)
          (magit-status dest))
      (magit-clone-regular url dest `(,(and branch `("-b" ,branch))
                                      ,@args)))))

(cl-defun akirak/git-default-clone-destination (repo &key create-parent)
  (let* ((branch (akirak/remote-git-repo-branch repo))
         (branch (if (member branch '("master" "main"))
                     nil
                   branch))
         (parent (akirak/git-clone-parent-select
                  (akirak/remote-git-repo-url repo)))
         (name (concat (akirak/remote-git-repo-name repo)
                       (if branch
                           (concat "@" branch)
                         ""))))
    (when (and create-parent
               (not (file-directory-p parent)))
      (make-directory parent t))
    (f-join parent name)))

;; Choose a parent directory based on magit-repository-directories
;; and git-identity-list.
(defun akirak/git-clone-parent-select (url)
  (require 'git-identity)
  (or (pcase (git-identity--guess-identity-by-url url)
        (`(domain ,domain ,ent)
         (when-let*
             ((roots (->> (plist-get (cdr ent) :dirs)
                          (-map (lambda (root)
                                  (->> magit-repository-directories
                                       (-map (lambda (ent)
                                               (let ((dir1 (expand-file-name (car ent)))
                                                     (dir2 (expand-file-name root)))
                                                 (when (string-prefix-p dir1 dir2)
                                                   (let ((rest (- (cdr ent)
                                                                  (length (f-split (string-remove-prefix dir1 dir2))))))
                                                     (when (>= rest 0)
                                                       (cons root rest)))))))
                                       (-non-nil))))
                          (-flatten-n 1)))
              (candidates (->> roots
                               (-map (pcase-lambda (`(,root . ,level))
                                       (pcase level
                                         (0 (list root))
                                         (1 (-map (lambda (x)
                                                    (f-slash (f-short x)))
                                                  (f-directories root)))
                                         (_ (error "Unsupported level: %d (root: %s)" level root)))))
                               (-flatten-n 1)))
              (choice (completing-read (format "Choose a parent directory for %s: " url)
                                       candidates))
              (y (or (when (member choice candidates)
                       choice)
                     (when-let (x (rassoc 1 roots))
                       (f-slash (f-short (f-join (car x) choice)))))))
           y)))
      (read-directory-name (format "Choose a parent directory for %s: " url)
                           "~/work/")))

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

(cl-defgeneric akirak/remote-git-repo-github-p (repo)
  nil)

(cl-defgeneric akirak/remote-git-repo-host-short-name (repo))
(cl-defgeneric akirak/remote-git-repo-abbreviate-url (repo)
  (ignore-errors
    (format "%s:%s/%s"
            (akirak/remote-git-repo-host-short-name repo)
            (akirak/remote-git-repo-owner repo)
            (akirak/remote-git-repo-name repo))))

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
(cl-defgeneric akirak/remote-git-repo-github-p ((repo akirak/github-https-repo))
  t)
(cl-defmethod akirak/remote-git-repo-host-short-name ((repo akirak/github-https-repo))
  "FIXME"
  "github")

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
(cl-defgeneric akirak/remote-git-repo-github-p ((repo akirak/github-ssh-repo))
  t)
(cl-defmethod akirak/remote-git-repo-host-short-name ((repo akirak/github-ssh-repo))
  "FIXME"
  "github")

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
(cl-defmethod akirak/remote-git-repo-protocol ((repo akirak/generic-git-repo))
  (or (akirak/generic-git-repo-protocol repo)
      (when-let (url (akirak/generic-git-repo-url repo))
        (save-match-data
          (cond
           ((string-match (rx bol "https://") url)
            "https")
           ((string-match (rx bol (or "git@" "ssh:")) url)
            "ssh")
           ((string-match (rx bol "hg::") url)
            "hg")
           (t
            (error "Failed to parse a protocol from %s" url)))))
      (error "Failed to extract a protocol from %s" repo)))
(cl-defmethod akirak/remote-git-repo-host ((repo akirak/generic-git-repo))
  (or (akirak/generic-git-repo-host repo)
      (when-let (url (akirak/generic-git-repo-url repo))
        (save-match-data
          (cond
           ((string-match (rx bol (? "hg::") "https://" (group (+ (not (any "/"))))) url)
            (match-string 1 url))
           ((string-match (rx bol (? "hg::") "git@" (group (+ (not (any ":"))))) url)
            (match-string 1 url))
           (t
            (error "Failed to parse a host from %s" url)))))
      (error "Failed to extract a host from %s" repo)))
(cl-defmethod akirak/remote-git-repo-name ((repo akirak/generic-git-repo))
  (or (akirak/generic-git-repo-name repo)
      (when-let (url (akirak/generic-git-repo-url repo))
        (save-match-data
          (when (string-match (rx (group (+ (not (any "/")))) (optional "/") eos)
                              url)
            (match-string 1 url))))))
(cl-defmethod akirak/remote-git-repo-clone-parent ((repo akirak/generic-git-repo))
  nil)
(cl-defmethod akirak/remote-git-repo-host-short-name ((repo akirak/generic-git-repo))
  (akirak/generic-git-repo-host repo))

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
         (https-pattern (concat "https://" host-pattern "/" path-pattern
                                (rx (? (or (and "/blob/" (group (+ (any alnum "-")))
                                                "/" (group (and (any alnum ".")
                                                                (+ anything))))
                                           ".git"
                                           "/"))
                                    eol)))
         (ssh-pattern (concat "git@" host-pattern ":" path-pattern (rx ".git" eol))))
    (pcase-let* ((`(,protocol ,url ,host ,owner ,name . ,rest)
                  (or (-some->> (s-match name-pattern string)
                        (append (list nil nil "github.com" akirak/github-login)))
                      (-some->> (s-match github-path-pattern string)
                        (cdr)
                        (append (list nil nil "github.com")))
                      (-some->> (s-match (concat "^" https-pattern) string)
                        (cons 'https))
                      (-some->> (s-match (concat "^" ssh-pattern) string)
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
       ;; This supports Mercurial repository URLs prefixed with hg::
       ;; You'll need git-remote-hg: <https://github.com/felipec/git-remote-hg>
       ((string-match-p (rx (or (and bol (? "hg::") (or "git:"
                                                        "git+ssh:"
                                                        "https://"
                                                        "git@"))
                                (and ".git" eol)))
                        string)
        (make-akirak/generic-git-repo :url string))
       (t
        (error "Looks like an invalid URL: %s" string))))))

;;;; Commands

;; Deprecated. Use `akirak/helm-giturl-dummy-action'.
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

(defun commonplace-repos-counsel-rg ()
  (interactive)
  (counsel-rg nil commonplace-repos-find-root))

(provide 'setup-git-clone)
