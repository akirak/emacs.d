;; -*- lexical-binding: t; -*-

(defun akirak/with-ensure-git-submodule (submodule git-submodule-args
                                                   on-update
                                                   func &rest args)
  "Ensure a submodule is active and then call a function.

SUBMODULE is an alist containing information of the submodule. An
item returned from `akirak/toplevel-repos-submodules' is an
example.

GIT-SUBMODULE-ARGS is a list of optional arguments passed to \"git
submodule update\" command.

ON-UPDATE is a function called if and only i the submodule is
updated. It is called before FUNC.

FUNC is called with ARGS."
  (let-alist submodule
    (if \.active
        (apply func args)
      (message "Updating submodule %s in %s..." \.name \.root)
      (let ((default-directory \.root))
        (magit-with-toplevel
          (make-process :name "git-submodule-update"
                        :command `("git"
                                   "submodule" "update"
                                   "--init"
                                   ,@git-submodule-args
                                   "--"
                                   ,\.name)
                        :sentinel
                        `(lambda (process event)
                           (when (eq (process-status process) 'exit)
                             (if (= (process-exit-status process) 0)
                                 (progn
                                   (message "Successfully run git submodule update")
                                   (when (functionp ,on-update)
                                     (let ((default-directory ,\.root))
                                       (funcall ,on-update))
                                     (funcall ,func ,@args)))
                               (user-error "Failed to check out %s in %s"
                                           ,\.name ,\.root))))))))))

(defun akirak/git-submodule-p ()
  "Return non-nil if the default directory is inside a submodule."
  (akirak/git-submodule-info :no-alist t))

(defun akirak/git-submodule-find-by-path (root path)
  (let ((path (if (and (string-prefix-p "repos/" path)
                       (file-equal-p root akirak/git-bookmark-repository))
                  (concat "repos-src/"
                          (string-remove-prefix "repos/" path))
                path)))
    (->> (akirak/git-submodules-parse (f-join root ".gitmodules"))
         (cl-find-if `(lambda (module-cell)
                        (string-equal (string-remove-suffix "/" ,path)
                                      (alist-get 'path (cdr module-cell))))))))

(cl-defun akirak/git-submodule-info (&key no-alist read-modules)
  "Return non-nil if the default directory is inside a submodule."
  (when-let* ((root1 (locate-dominating-file default-directory ".git"))
              (root2 (locate-dominating-file (f-parent root1) ".git")))
    (if no-alist
        t
      (let* ((path (f-relative root1 root2))
             (alist `((root . ,root2)
                      (path . ,path))))
        (if read-modules
            (let ((cell (or (akirak/git-submodule-find-by-path root2 path)
                            (error "Cannot find a module with path %s in %s"
                                   path root2))))
              (append (cons (cons 'name (car cell))
                            alist)
                      (cdr cell)))
          alist)))))

(defun akirak/git-module-tags (pairs)
  (->> (cl-remove 'tags pairs :key #'car :test-not #'eq)
       (-map #'cdr)
       (mapcar (lambda (s) (split-string s ",")))
       (-flatten-n 1)))

(defun akirak/git-remote-topics (repo)
  (cl-typecase repo
    (string (akirak/git-remote-tags (akirak/parse-git-url repo)))
    (akirak/remote-git-repo-github (akirak/github-repo-topics
                                    (akirak/remote-git-repo-owner repo)
                                    (akirak/remote-git-repo-name repo)))
    (otherwise nil)))

(defun akirak/git-module-add-tags ()
  (interactive)
  (if-let ((module (akirak/git-submodule-info :read-modules t)))
      (let-alist module
        (let* ((default-directory \.root)
               (current-tags (akirak/git-module-tags module))
               (new-tags (string-trim
                          (read-string (format "Add tags to %s [%s]: "
                                               \.name
                                               (if current-tags
                                                   (string-join current-tags
                                                                ",")
                                                 "nil"))
                                       (unless current-tags
                                         (string-join (akirak/git-remote-topics \.url)
                                                      ","))))))
          (cl-assert (string-match-p (rx bol (+ (any "-," alnum)) eol)
                                     new-tags))
          ;; Run git config -f .gitmodules --add module.name.tags new-tags
          (call-process-with-args "git" "config" "-f" ".gitmodules"
            "--add" (format "submodule.%s.tags" \.name)
            new-tags)))
    (user-error "Not a module")))

(provide 'my/gitmodule/function)
