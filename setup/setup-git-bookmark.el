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
  (lambda (obj)
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
  (when (or (and (buffer-file-name)
                 (equal "LICENSE" (file-name-base (buffer-file-name))))
            (let* ((root (locate-dominating-file default-directory ".git"))
                   (files (and root
                               (directory-files root t
                                                (rx bol "LICENSE" (optional "." (+ anything)) eol)))))
              (cond
               (files
                (find-file (car files))
                nil)
               ((not root)
                (user-error "Not inside a repository"))
               (t
                (yes-or-no-p (format "No license is found in %s. Are you sure you want to bookmark this repository?"
                                     root))))))
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
           (default-directory akirak/git-bookmark-repository)
           (visited-directory (f-join akirak/git-bookmark-repository
                                      (concat
                                       "repos/"
                                       (string-remove-prefix
                                        "repos-src/"
                                        submodule-rel-path)))))
      (when (file-directory-p submodule-abs-path)
        (user-error "%s already exists" submodule-abs-path))
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
                                 (dired ,visited-directory))
                             (error "Error while cloning a submodule: %s"
                                    event)))))))))

(defun akirak/git-bookmark-ensure-file-funcall (file func &rest args)
  "Ensure the submodule containing FILE if any and then call FUNC with ARGS."
  (declare (indent 1))
  (if (and (not (file-exists-p file))
           (f-ancestor-of-p (f-join akirak/git-bookmark-repository "repos")
                            file))
      (let* ((relative (s-replace "repos/" "repos-src/"
                                  (f-relative file akirak/git-bookmark-repository)))
             (submodules (akirak/git-submodules-full-alist
                          akirak/git-bookmark-repository))
             (submodule (cl-find-if
                         `(lambda (submodule)
                            (string-prefix-p
                             (alist-get 'path submodule)
                             ,relative))
                         submodules)))
        (apply #'akirak/with-ensure-git-submodule
               submodule
               '("--depth" "1")
               #'akirak/git-bookmark-update-function
               func args))
    (apply func args)))

(advice-add #'bookmark-default-handler
            :around
            (defun akirak/bookmark-default-handler (orig record)
              (akirak/git-bookmark-ensure-file-funcall
                  (bookmark-get-filename record)
                orig record)))

(advice-add #'org-link-open-as-file
            :around
            (cl-defun akirak/git-bookmark-ad-around-org-link-open-as-file
                (orig path &rest rest)
              (apply #'akirak/git-bookmark-ensure-file-funcall
                     path
                     orig path rest)))

(provide 'setup-git-bookmark)
