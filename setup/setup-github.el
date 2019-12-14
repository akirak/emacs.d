(defvar akirak/github-repository-history nil)

(defun akirak/interesting-github-repos (&rest _args)
  ;; TODO: Collect more information
  akirak/github-repository-history)

(defun akirak/select-github-repo (&optional prompt)
  (completing-read (or prompt "GitHub repository: ")
                   (akirak/interesting-github-repos)
                   nil nil nil
                   'akirak/github-repository-history))

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

(provide 'setup-github)
