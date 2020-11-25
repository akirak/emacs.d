;; TODO: add a function for inspect the license of a GitLab repository

(defun akirak/visit-license (&optional root)
  (let ((root (or root (car (project-roots (project-current))))))
    (if-let (files (directory-files root t
                                    (rx bol "LICENSE" (optional "." (+ anything)) eol)))
        (find-file (car files))
      (error "No license is found in %s" root))))

(defcustom akirak/git-bookmark-repository "~/commonplace/"
  "Git repository that contains repository bookmarks as submodules."
  :type 'file)

(defcustom akirak/git-bookmark-submodule-path-function
  (lambda (repo)
    (let ((host (cond
                 ((or (akirak/github-https-repo-p obj)
                      (akirak/github-ssh-repo-p obj))
                  "github.com")
                 ((akirak/generic-git-repo-p obj)
                  (akirak/generic-git-repo-host obj))))
          (owner (akirak/remote-git-repo-owner obj))
          (name (akirak/remote-git-repo-name obj)))
      (cl-assert host)
      (cl-assert owner)
      (cl-assert name)
      (cl-assert (or (member host '("github.com" "gitlab.com"))
                     (yes-or-no-p (format "Are you sure you want to bookmark a repository on %s?" host))))
      ;; TODO: Add support for generic URLs without owner ane name
      (f-join "repos-src" host owner name)))
  "Function used to determine the submodule path of a bookmark."
  :type 'function)

(defcustom akirak/git-bookmark-update-function
  (lambda ()
    (call-process-with-args "systemctl"
      "--user" "restart" "commonplace-repos.service"))
  "Function called "
  :type 'function)

(defun akirak/git-bookmark-repository ()
  (interactive)
  ;; Check the license before bookmarking it
  (if (not (and (buffer-file-name)
                (equal "LICENSE" (file-name-base (buffer-file-name)))))
      (akirak/visit-license)
    (cl-assert (file-directory-p akirak/git-bookmark-repository))
    (cl-assert commonplace-repos-clone-root)
    (let* ((origin (magit-git-string "remote" "get-url" "origin"))
           (obj (akirak/parse-git-url origin))
           (obj (cond
                 ((akirak/github-ssh-repo-p obj)
                  (make-akirak/github-https-repo
                   :owner (akirak/remote-git-repo-owner obj)
                   :name (akirak/remote-git-repo-name obj)))
                 (t obj)))
           ;; (f-join (f-relative commonplace-repos-clone-root
           ;;                     akirak/git-bookmark-repository)
           ;;         path-with-host)
           (submodule-rel-path (funcall akirak/git-bookmark-submodule-path-function obj))
           (submodule-abs-path (f-join akirak/git-bookmark-repository submodule-rel-path))
           (url (akirak/remote-git-repo-url obj))
           (default-directory akirak/git-bookmark-repository))
      (when (file-directory-p submodule-abs-path)
        (user-error "%s already exists. You've probably added it" submodule-abs-path))
      (message "Cloning %s as a submodule" url)
      ;; I won't use `magit-submodule-add-1' because I want to
      ;; alternate the process sentinel.
      (magit-with-toplevel
        (make-process :name "git-submodule-add"
                      :command (list "git"
                                     "submodule" "add"
                                     "--depth" "1"
                                     "--"
                                     url
                                     submodule-rel-path)
                      :sentinel
                      `(lambda (process event)
                         (when (eq (process-status process) 'exit)
                           (if (= (process-exit-status process) 0)
                               (progn
                                 (funcall akirak/git-bookmark-update-function)
                                 (dired (f-join akirak/git-bookmark-repository
                                                "repos"
                                                (string-remove-prefix
                                                 "repos-src"
                                                 ,submodule-rel-path))))
                             (error "Error while cloning a submodule: %s"
                                    event)))))))))

(provide 'setup-git-bookmark)