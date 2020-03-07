;; -*- lexical-binding: t -*-
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

;;;; Fetching information via GitHub API

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

(cl-defun akirak/ghub-with-cache (endpoint cache-var
                                           &key params force
                                           unpaginate
                                           (method "GET"))
  (declare (indent 1))
  (promise-new
   (lambda (resolve _reject)
     (if-let ((immediate (and (not force)
                              (symbol-value cache-var))))
         (funcall resolve immediate)
       (message "Waiting for a response from GitHub...")
       (let ((start (ts-now))
             (result (ghub-request method endpoint params
                                   :unpaginate unpaginate
                                   :auth 'ghub)))
         (when result
           (set cache-var result)
           (message "Finished in %s sec" (ts-difference (ts-now) start))
           (funcall resolve result)))))))

(cl-defun akirak/github-repo-format-candidate (data)
  (let ((full_name (alist-get 'full_name data))
        (description (alist-get 'description data))
        (language (alist-get 'language data))
        (private (alist-get 'private data))
        (fork (alist-get 'fork data))
        (archived (alist-get 'archived data)))
    (string-join (delq nil
                       (list full_name
                             (propertize (format "[%s]" language)
                                         'face 'font-lock-type-face)
                             (propertize description
                                         'face 'font-lock-comment-face))))))

(defclass akirak/github-repos-helm-source-class (helm-source-sync)
  ((action :initform 'akirak/github-repos-helm-actions)
   (candidate-transformer
    :initform
    (lambda (items)
      (mapcar #'akirak/github-repo-format-candidate items)))))

(defvar akirak/github-starred-repos-cache nil)

(defvar akirak/github-starred-repos-helm-source
  (helm-make-source "Starred repos:"
      'akirak/github-repos-helm-source-class
    :candidates 'akirak/github-starred-repos-cache))

(defun akirak/browse-github-starred-repos (&optional arg)
  (interactive "P")
  (require 'promise)
  (promise-chain (akirak/ghub-with-cache
                     (format "/users/%s/starred" akirak/github-login)
                   'akirak/github-starred-repos-cache
                   :force arg)
    (then (lambda (_repos)
            (helm :prompt "Starred GitHub repo: "
                  :sources akirak/github-starred-repos-helm-source)))))

(defvar akirak/github-user-repos-cache nil)

(defvar akirak/github-user-repos-helm-source
  (helm-make-source "User repos:"
      'akirak/github-repos-helm-source-class
    :candidates (lambda () akirak/github-user-repos-cache)))

(defun akirak/browse-github-user-repos (&optional arg)
  (interactive "P")
  (require 'promise)
  (promise-chain (akirak/ghub-with-cache
                     (format "/users/%s/repos" akirak/github-login)
                   'akirak/github-user-repos-cache
                   :force arg)
    (then (lambda (repos)
            ;; (helm :prompt "User GitHub repo: "
            ;;       :sources
            ;;       (helm-make-source "User repos:"
            ;;           'akirak/github-repos-helm-source-class
            ;;         :candidates repos))
            (completing-read "User repos: "
                             (-map (lambda (repo) (alist-get 'full_name repo))
                                   repos))))))

(akirak/bind-admin
  "gs" #'akirak/browse-github-starred-repos)

(provide 'setup-github)
