;; URL patterns are stolen from my playground package:
;; https://github.com/akirak/emacs-playground/blob/master/playground.el
(defconst akirak/github-repo-path-pattern
  "\\(?:[0-9a-z][-0-9a-z]+/[-a-z0-9_.]+?[0-9a-z]\\)"
  "A regular expression for a repository path (user/repo) on GitHub.")

(defconst akirak/github-https-url-regexp
  (concat "^https://github\.com/\\("
          akirak/github-repo-path-pattern
          "\\)\\(\.git\\)?/?$"))

(defcustom akirak/default-git-clone-directory "~/tmp"
  "Directory to which Git repositories are cloned.")

(defun akirak/git-clone (url)
  (interactive (list (read-string "Repository URL: ")))
  (-when-let ((url . name)
              (or (when (string-match akirak/github-https-url-regexp url)
                    (let ((path (match-string 1 url)))
                      (cons (format "https://github.com/%s.git" path)
                            (file-name-nondirectory path))))
                  (when (string-match (concat "^" akirak/github-repo-path-pattern "$") url)
                    (let ((path (match-string 1 url)))
                      (cons (format "https://github.com/%s.git" path)
                            (file-name-nondirectory path))))
                  (when (string-match (rx "/" (group (1+ (any "-" alnum))) ".git" bol) url)
                    (cons url (match-string 1 url)))
                  (user-error "Doesn't seem to be a Git repository URL: %s" url)))
    (akirak//git-clone-default-directory url name)))

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

(provide 'setup-git-clone)
