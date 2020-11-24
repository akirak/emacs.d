(defvar akirak/github-repository-history nil)

(defun akirak/interesting-github-repos (&rest _args)
  ;; TODO: Collect more information
  akirak/github-repository-history)

(defun akirak/select-github-repo (&optional prompt)
  (completing-read (or prompt "GitHub repository: ")
                   (akirak/interesting-github-repos)
                   nil nil nil
                   'akirak/github-repository-history))

(use-package treepy)

(use-package forge
  :config
  (akirak/bind-browse-at-remote
    "w" #'forge-browse-dwim
    "RET" #'forge-browse-post
    "c" #'forge-browse-commit
    "t" #'forge-browse-topic
    "I" #'forge-browse-issues
    "P" #'forge-browse-pullreqs))

(use-package github-review
  :commands (github-review-forge-pr-at-point))

(defun akirak/browse-github-readme (repo &optional ref)
  (interactive (list (akirak/select-github-repo "README of GitHub repo: ")))
  (let* ((endpoint (concat (format "/repos/%s/readme" repo)
                           (if ref
                               (concat "?ref=" ref)
                             "")))
         (response (ghub-get endpoint nil :auth 'ghub))
         (name (alist-get 'name response))
         (encoding (alist-get 'encoding response))
         (encoded-content (alist-get 'content response))
         (url (alist-get 'download_url response))
         (mode (cdr (cl-find-if
                     (lambda (pattern)
                       (string-match-p pattern name))
                     auto-mode-alist :key #'car)))
         (content (cond
                   ((equal encoding "base64")
                    (base64-decode-string encoded-content))
                   (t
                    (error "Failed to decode the content"))))
         (buffer (or (get-buffer url)
                     (with-current-buffer (generate-new-buffer url)
                       (insert content)
                       (when mode
                         (funcall mode)
                         (run-hooks 'after-change-major-mode-hook))
                       (goto-char (point-min))
                       (when (eq mode 'org-mode)
                         (org-global-cycle 2))
                       (current-buffer))))
         (window (or (get-buffer-window buffer t)
                     (display-buffer-pop-up-frame
                      buffer nil))))
    (if window
        (progn
          (select-window window)
          ;; (when (featurep 'imenu-list)
          ;;   (with-selected-window window
          ;;     (imenu-list-show)))
          )
      (pop-to-buffer buffer))))

(defun akirak/github-repo-path-with-default ()
  (or (ignore-errors
        (cadr (s-match (rx "github.com" (or ":" "/")
                           (group (+? anything))
                           (optional ".git") eol)
                       (magit-git-string "config" "remote.origin.url"))))
      (ignore-errors
        (concat (magit-git-string "config" "github.user")
                "/"
                (f-filename (car-safe (project-roots (project-current))))))))

;;;; Browsing repositories

(cl-defun akirak/browse-github-repos (prompt items
                                             &key omit-user)
  (cl-labels
      ((format-item
        (x)
        (let-alist x
          (list
           (format "%s%s%s %s %s %s %s"
                   (propertize (if omit-user
                                   \.name
                                 \.full_name)
                               'face 'font-lock-string-face)
                   (if \.fork
                       (propertize " (fork)"
                                   'face 'font-lock-constant-face)
                     "")
                   (if \.private
                       (propertize " (private)"
                                   'face 'font-lock-constant-face)
                     "")
                   (propertize (format "(%d stars)"
                                       \.stargazers_count)
                               'face 'font-lock-comment-face)
                   (or \.description
                       "(no description)")
                   (propertize (or \.language "")
                               'face 'font-lock-type-face)
                   (propertize (ts-human-format-duration
                                (ts-difference
                                 (ts-now)
                                 (make-ts
                                  :unix (float-time (parse-iso8601-time-string
                                                     \.updated_at))))
                                t)
                               'face 'font-lock-comment-face))
           (cons 'clone_url \.clone_url)
           (cons 'html_url \.html_url)
           (cons 'issues_url \.issues_url)
           (cons 'pull_url \.pull_url)))))
    (helm :sources
          (helm-build-sync-source prompt
            :candidates (-map #'format-item items)
            :action
            (quote (("Clone" .
                     (lambda (x)
                       (akirak/git-clone-remote-repo (alist-get (quote clone_url) x))))
                    ("Browse" .
                     (lambda (x)
                       (akirak/browse-url (alist-get 'html_url x))))
                    ("Browse issues on browser" .
                     (lambda (x)
                       (akirak/browse-url (alist-get 'issues_url x))))
                    ("Browse PRs on browser" .
                     (lambda (x)
                       (akirak/browse-url (alist-get 'pulls_url x))))))))))

(defun akirak/github-recent-repos ()
  (interactive)
  (akirak/browse-github-repos "Recent repos: "
                              (ghub-get "/user/repos"
                                        '((sort . "updated")
                                          (per_page . 5)))
                              :omit-user t))

(defvar akirak/github-api-v3-cache nil)

(cl-defmacro akirak/github-v3-get-cached (path &optional params
                                               &key unpaginate force)
  (declare (indent 1))
  `(let* ((cached (assoc ,path akirak/github-api-v3-cache))
          (result (or (and (not ,force) (cdr cached))
                      (let ((start (ts-now)))
                        (message "Requesting to github '%s' %s..."
                                 ,path (or ,params ""))
                        (prog1 (ghub-get ,path ,params
                                         :unpaginate ,unpaginate)
                          (message "Received response on %s in %.1fsec"
                                   ,path
                                   (ts-difference (ts-now) start)))))))
     (cond
      ((and ,force cached)
       (setcdr cached result))
      ((not cached)
       (push (cons ,path result) akirak/github-api-v3-cache)))
     result))

(defun akirak/github-user-repos (&optional user force)
  (interactive (list (when current-prefix-arg
                       (read-string "User: "))
                     (equal current-prefix-arg '(16))))
  (akirak/browse-github-repos (if user
                                  (format "%s's repos: " user)
                                "User repos: ")
                              (akirak/github-v3-get-cached
                                  (if user
                                      (format "/users/%s/repos" user)
                                    "/user/repos")
                                '((sort . "updated"))
                                :unpaginate t
                                :force force)
                              :omit-user t))

(defun akirak/github-owned-repos (&optional arg)
  (interactive "P")
  (if arg
      (akirak/github-user-repos)
    (akirak/github-recent-repos)))

(defun akirak/github-starred-repos (&optional arg)
  (interactive "P")
  (akirak/browse-github-repos "Starred repos: "
                              (akirak/github-v3-get-cached "/user/starred"
                                nil
                                :force arg)))

(defun akirak/github-users ()
  (interactive)
  (let* ((following (akirak/github-v3-get-cached "/user/following"
                      nil
                      :unpaginate t))
         (user (completing-read "GitHub users: "
                                (-map (lambda (x) (alist-get 'login x))
                                      following))))
    (akirak/github-user-repos user)))

(defvar akirak/github-stars-getting nil)

(defun akirak/github-download-all-stars ()
  (interactive)
  (if akirak/github-stars-getting
      (message "Already started")
    (setq akirak/github-stars-getting t)
    (make-thread
     (lambda ()
       (message "Started getting stars in the background...")
       (akirak/github-v3-get-cached "/user/starred"
         nil
         :unpaginate t
         :force t)
       (setq akirak/github-stars-getting nil)))))

(defconst akirak/github-user-query
  "query {
  user(login: \"%s\") {
    following(first: 30%s) {
      edges {
        node {
          login
          websiteUrl
          bioHTML
          company
          twitterUsername
          location
          name
        }
        cursor
      }
    }
  }
}")

(defun akirak/github--following ()
  (or (bound-and-true-p akirak/github-following-list)
      (let ((user (car (magit-config-get-from-cached-list "github.user")))
            result
            chunk
            cursor)
        (catch 'finish
          (while (setq chunk
                       (->> (ghub-graphql (format akirak/github-user-query
                                                  user
                                                  (if cursor
                                                      (format ", after: \"%s\"" cursor)
                                                    "")))
                            (alist-get 'data)
                            (alist-get 'user)
                            (alist-get 'following)
                            (alist-get 'edges)))
            (message "%d users fetched" (length result))
            (setq result (append result
                                 (-map (lambda (x)
                                         (alist-get 'node x))
                                       chunk)))
            (setq cursor (alist-get 'cursor (-last-item chunk)))))
        (message "Finished retrieving all users you follow")
        (setq akirak/github-following-list result))))

(defcustom akirak/github-auto-get-stars nil
  "Whether to start getting GitHub stars at startup."
  :type 'boolean)

(when akirak/github-auto-get-stars
  (add-hook 'emacs-startup-hook
            (lambda ()
              (sleep-for 5)
              (akirak/github-download-all-stars))))

(defun akirak/insert-github-action-html-badge ()
  (interactive)
  (let* ((origin (car (magit-config-get-from-cached-list "remote.origin.url")))
         (repo (akirak/parse-git-url origin))
         (url (concat "https://github.com/"
                      (akirak/remote-git-repo-owner repo)
                      "/"
                      (akirak/remote-git-repo-name repo)))
         (root (vc-git-root default-directory))
         (workflows (-map (lambda (file)
                            (with-temp-buffer
                              (insert-file-contents file)
                              (goto-char (point-min))
                              (and (re-search-forward (rx bol "name: ") nil t)
                                   (let ((s (buffer-substring-no-properties
                                             (point)
                                             (line-end-position))))
                                     (save-match-data
                                       (and (string-match (rx bol (?  (any "\"'"))
                                                              (group (+? anything))
                                                              (?  (any "\"'")) eol)
                                                          s)
                                            (match-string 1 s)))))))
                          (directory-files (f-join root ".github" "workflows")
                                           t (rx ".yml" eol))))
         (name (completing-read "Workflow name: " workflows))
         (branch (magit-read-local-branch "Branch: ")))
    (insert (concat (format "<a href=\"%s/actions?query=workflow%%3A%%22%s%%22+branch%%3A%s\">\n"
                            url
                            (shr-encode-url (s-replace " " "+" name))
                            (shr-encode-url branch))
                    (format "<img alt=\"Build Status\" src=\"%s/workflows/%s/badge.svg?branch=%s\" />\n"
                            url
                            (shr-encode-url name)
                            (shr-encode-url branch))
                    "</a>"))))

(defun akirak/github-build-search-url (query type)
  (format "https://github.com/search?q=%s&type=%s%s"
          (shr-encode-url query)
          (cl-ecase type
            (:repositories "repositories")
            (:code "code"))
          ;; Add optional parameters depending on the type
          (cl-case type
            (:repositories "&s=stars")
            (otherwise ""))))

(defvar akirak/github-search-query-history nil)

(defun akirak/github-search-repository (query)
  (interactive (list (read-string "Search repository: "
                                  nil
                                  'akirak/github-search-query-history)))
  (add-to-list 'akirak/github-search-query-history query)
  (akirak/browse-url (akirak/github-build-search-url query :repositories)))

(defun akirak/github-search-code (query)
  (interactive (list (read-string "Search code: "
                                  nil
                                  'akirak/github-search-query-history)))
  (add-to-list 'akirak/github-search-query-history query)
  (akirak/browse-url (akirak/github-build-search-url query :code)))

(defun akirak/github-search-starred-repos (query)
  (interactive (list (read-string "Search starred repository: "
                                  nil
                                  'akirak/github-search-query-history)))
  (add-to-list 'akirak/github-search-query-history query)
  (akirak/browse-url (format "https://github.com/akirak?tab=stars&q=%s"
                             (shr-encode-url query))))

(defun akirak/github-search-ivy ()
  (interactive)
  (ivy-read "Search on GitHub: "
            akirak/github-search-query-history
            :caller 'akirak/github-search-ivy
            :history 'akirak/github-search-query-history
            :action #'akirak/github-search-repository))

(ivy-add-actions 'akirak/github-search-ivy
                 '(("r" akirak/github-search-repository "Search repository")
                   ("c" akirak/github-search-code "Search code")
                   ("s" akirak/github-search-starred-repos "Search stars")))

(akirak/bind-search "M-g" #'akirak/github-search-ivy)

(provide 'setup-github)
