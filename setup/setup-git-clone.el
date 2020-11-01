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

(cl-defgeneric akirak/remote-git-repo-clone-default (repo)
  (let ((url (akirak/remote-git-repo-url repo))
        (dest (f-join (akirak/remote-git-repo-clone-parent repo)
                      (akirak/remote-git-repo-name repo)))
        (args (transient-args 'magit-clone)))
    (if (file-directory-p dest)
        (progn
          (message "Already exists: %s" dest)
          (magit-status dest))
      (magit-clone-regular url dest args))))

(cl-defgeneric akirak/remote-git-repo-url-github-p (_)
  nil)

(cl-defgeneric akirak/remote-git-repo-url (repo))
(cl-defgeneric akirak/remote-git-repo-owner (repo))
(cl-defgeneric akirak/remote-git-repo-name (repo))
(cl-defgeneric akirak/remote-git-repo-clone-parent (repo))

;;;; GitHub repositories (https)

(cl-defstruct akirak/github-https-repo owner name)
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
(cl-defgeneric akirak/remote-git-repo-url-github-p ((repo akirak/github-https-repo))
  t)

;;;; GitHub repositories (ssh)

(cl-defstruct akirak/github-ssh-repo owner name)
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
(cl-defgeneric akirak/remote-git-repo-url-github-p ((repo akirak/github-ssh-repo))
  t)

;;;; Generic Git repositories

(cl-defstruct akirak/generic-git-repo url protocol host owner name)
(cl-defmethod akirak/remote-git-repo-url ((repo akirak/generic-git-repo))
  (akirak/generic-git-repo-url repo))
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
                           (group (+ (any alnum "-_.")))))
         (host-pattern (rx (group (+ (not (any "./")))
                                  (+ (and "." (+ (not (any "./"))))))))
         (suffix (rx ".git"))
         (optional-suffix (rx (?  ".git")))
         (github-path-pattern (concat "^" path-pattern "$"))
         (https-pattern (concat "^https://" host-pattern "/" path-pattern
                                optional-suffix))
         (ssh-pattern (concat "^git@" host-pattern ":" path-pattern suffix "$")))
    (pcase-let ((`(,protocol ,url ,host ,owner ,name)
                 (or (-some->> (s-match name-pattern string)
                       (append (list nil nil "github.com" akirak/github-login)))
                     (-some->> (s-match github-path-pattern string)
                       (cdr)
                       (append (list nil nil "github.com")))
                     (-some->> (s-match https-pattern string)
                       (cons 'https))
                     (-some->> (s-match ssh-pattern string)
                       (cons 'ssh)))))
      (cond
       ((and (equal host "github.com") (equal owner akirak/github-login))
        (make-akirak/github-ssh-repo :owner owner
                                     :name (string-remove-suffix ".git" name)))
       ((equal host "github.com")
        (make-akirak/github-https-repo :owner owner
                                       :name (string-remove-suffix ".git" name)))
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

(provide 'setup-git-clone)
