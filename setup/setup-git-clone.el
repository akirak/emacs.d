;; URL patterns are stolen from my playground package:
;; https://github.com/akirak/emacs-playground/blob/master/playground.el
(defconst akirak/github-repo-path-pattern
  "\\(?:[0-9a-z][-0-9a-z]+/[-a-z0-9_.]+?[0-9a-z]\\)"
  "A regular expression for a repository path (user/repo) on GitHub.")

(defconst akirak/github-https-url-regexp
  (concat "^https://github\.com/\\("
          akirak/github-repo-path-pattern
          "\\)\\(\.git\\)?/?$"))

(defcustom akirak/git-clone-default-directory "~/tmp"
  "Default directory in which Git repositories are created.")

(defcustom akirak/git-clone-user-directory "~/projects/github"
  "Default directory in which your own Git repositories are created.")

(defcustom akirak/github-login "akirak"
  "The user name on GitHub.")

(defun akirak/git-clone-github-repo (path)
  (interactive (list (read-string "GitHub repo: ")))
  (-let (((user repo) (s-split-up-to "/" path 1)))
    (cond
     ((string-equal user akirak/github-login)
      (akirak/git-clone-internal (concat "git@github.com:" path ".git")
                                 (expand-file-name repo
                                                   akirak/git-clone-user-directory)))
     (t
      (akirak/git-clone-some-repo (format "https://github.com/%s.git" path) repo)))))

(defun akirak/git-clone-some-repo (url name)
  (let ((parent (read-directory-name "Parent directory: " "~/")))
    (akirak/git-clone-internal url (expand-file-name name parent))))

(defun akirak/git-clone (url)
  (interactive (list (read-string "Repository URL: ")))
  (cond
   ((string-match akirak/github-https-url-regexp url)
    (akirak/git-clone-github-repo (match-string 1 url)))
   ((string-match (concat "^" akirak/github-repo-path-pattern "$") url)
    (akirak/git-clone-github-repo (match-string 1 url)))
   ((string-match (rx "/" (group (1+ (any "-" alnum))) ".git" bol) url)
    (akirak/git-clone-some-repo url (match-string 1 url)))))

(defun akirak/git-clone-internal (url local-path)
  "Call `magit-clone' on URL if LOCAL-PATH does not exist or otherwise open it."
  (if (file-exists-p local-path)
      (let ((default-directory local-path))
        (message "%s already exists" local-path)
        (funcall projectile-switch-project-action))
    (message "Cloning %s to %s..." url local-path)
    (magit-clone-regular url local-path nil)))

(defun akirak//git-clone-default-directory (url name)
  (let ((local-repo-path (expand-file-name name akirak/default-git-clone-directory)))
    (when (file-exists-p local-repo-path)
      (user-error "%s already exists" local-repo-path))
    (magit-clone url local-repo-path)))

(defun akirak/straight-use-package-git-url (url)
  (when-let ((recipe
              (or (when (string-match akirak/github-https-url-regexp url)
                    (let ((path (match-string 1 url)))
                      `(,(intern (file-name-nondirectory path))
                        :host github
                        :repo ,path)))
                  (when (string-match (concat "^" akirak/github-repo-path-pattern "$") url)
                    (let ((path (match-string 1 url)))
                      `(,(intern (file-name-nondirectory path))
                        :host github
                        :repo ,path)))
                  (when (string-match (rx "/" (group (1+ (any "-" alnum))) ".git" bol) url)
                    `(,(intern (match-string 1 url))
                      :type git
                      :url ,url))
                  (user-error "Doesn't seem to be a Git repository URL: %s" url))))
    (straight-use-package recipe)))

(defcustom akirak/git-clone-parent-directories nil
  "List of parent directories."
  :type '(repeat string))

(defun akirak/git-clone-parent-directories ()
  (append (thread-last projectile-known-projects
            (-map (-partial (-flip #'string-trim-right) "/"))
            (-group-by #'f-parent)
            (-sort (-on #'> #'seq-length))
            (-map #'car)
            (-map #'f-short)
            (-filter (-partial #'string-prefix-p "~")))
          akirak/git-clone-parent-directories))

(defun akirak/get-repository-name (url)
  (nth 1 (s-match (rx (or (and (group-n 1 (+ (not (any "/")))) ".git")
                          (group-n 1 (+ (not (any "/")))))
                      eos)
                  url)))

(defun akirak/git-clone-read-destination (url)
  (let ((parent (completing-read (format-message "Choose a parent directory for %s: " url)
                                 (akirak/git-clone-parent-directories)
                                 nil nil)))
    (if (and parent (not (string-empty-p parent)))
        (progn
          (unless (file-directory-p parent)
            (if (yes-or-no-p (format-message "Directory %s does not exist. Create a directory?" parent))
                (make-directory parent t)
              (user-error "Directory %s does not exist" parent)))
          (let ((name (read-string "Name: " (akirak/get-repository-name url))))
            (f-join parent name)))
      (read-directory-name (format-message "Clone the destination directory for %s: "
                                           url)
                           (car (akirak/git-clone-parent-directories))))))

(defun akirak/git-clone-url (url)
  (require 'magit-clone)
  (let ((directory (akirak/git-clone-read-destination url))
        (args (transient-args 'magit-clone)))
    (magit-clone-internal url directory args)))

(provide 'setup-git-clone)
