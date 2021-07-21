;; TODO: add a function for inspect the license of a GitLab repository

(defun akirak/visit-license (&optional root)
  (let ((root (or root (car (project-roots (project-current))))))
    (if-let (files (directory-files root t
                                    (rx bol "LICENSE" (optional "." (+ anything)) eol)))
        (find-file (car files))
      (error "No license is found in %s" root))))

(defcustom akirak/git-archive-repository "~/archives/commons/git/"
  "Location for cloning Git repositories for referencing."
  :type 'directory)

(defun akirak/git-archive-path (obj)
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
                   (yes-or-no-p (format "Are you sure you want to archive a repository on %s?" host))))
    ;; TODO: Add support for generic URLs without owner ane name
    (f-join host owner name)))

(cl-defun akirak/archive-public-git-repo (url &key branch)
  (interactive "sUrl: ")
  (let* ((obj (akirak/parse-git-url url))
         (obj (cond
               ((akirak/github-ssh-repo-p obj)
                (make-akirak/github-https-repo
                 :owner (akirak/remote-git-repo-owner obj)
                 :name (akirak/remote-git-repo-name obj)))
               (t obj)))
         (rel-path (akirak/git-archive-path obj))
         (abs-path (f-join akirak/git-archive-repository rel-path))
         (url (akirak/remote-git-repo-url obj))
         (parent (f-parent abs-path)))
    (if (file-directory-p abs-path)
        (progn
          (message "%s already exists" abs-path)
          (dired abs-path))
      (message "Cloning %s to %s..." url (f-short abs-path))
      (unless (file-directory-p parent)
        (make-directory parent t))
      (let ((default-directory parent))
        (make-process :name "git-clone"
                      :command `("git" "clone"
                                 ,@(when branch
                                     (list "-b" branch))
                                 "--filter=blob:none"
                                 "--recurse-submodules"
                                 ,url
                                 ,abs-path)
                      :sentinel
                      `(lambda (process event)
                         (when (eq (process-status process) 'exit)
                           (if (= (process-exit-status process) 0)
                               (progn
                                 (akirak/org-capture-archived-public-git-repo
                                  ,abs-path ',(f-split rel-path))
                                 (dired ,abs-path))
                             (error "Error while cloning a submodule: %s"
                                    event)))))))))

(defun akirak/org-capture-archived-public-git-repo (root olp)
  "Register a project to the repository archive."
  (let ((file (org-starter-locate-file "code.org" nil t)))
    (with-current-buffer (or (find-buffer-visiting file)
                             (find-file-noselect file))
      (org-with-wide-buffer
       (let ((ancestors (cons "Resources" (-butlast olp))))
         (catch 'finish
           (dolist (prefix (-non-nil (-inits ancestors)))
             (condition-case nil
                 (goto-char (org-find-olp prefix t))
               (error
                (let ((rest (-drop (1- (length prefix)) ancestors))
                      (level (org-outline-level)))
                  (org-back-to-heading)
                  (org-end-of-meta-data t)
                  (org-open-line 1)
                  (dolist (seg rest)
                    (setq level (org-get-valid-level (1+ level)))
                    (insert (make-string level ?*) " " seg "\n"))
                  (throw 'finish t))))))
         (octopus-register-project root :noninteractive t))))))

;; FIXME
(defun akirak/git-archive-ensure-file-funcall (file func &rest args)
  "Ensure the submodule containing FILE if any and then call FUNC with ARGS."
  (declare (indent 1))
  (if (and (not (file-exists-p file))
           (f-ancestor-of-p (f-join akirak/git-archive-repository "repos")
                            file))
      (let* ((relative (s-replace "repos/" "repos-src/"
                                  (f-relative file akirak/git-archive-repository)))
             (submodules (akirak/git-submodules-full-alist
                          akirak/git-archive-repository))
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
              (akirak/git-archive-ensure-file-funcall
                  (bookmark-get-filename record)
                orig record)))

(advice-add #'org-link-open-as-file
            :around
            (cl-defun akirak/git-bookmark-ad-around-org-link-open-as-file
                (orig path &rest rest)
              (apply #'akirak/git-archive-ensure-file-funcall
                     path
                     orig path rest)))

(provide 'setup-git-archive)
