(use-package org-web-tools)

(defun akirak/read-local-html-as-org (file)
  (-let* ((file (thread-last file
                  (string-remove-prefix "file:///")
                  (string-remove-prefix "file:/")))
          (html (with-temp-buffer
                  (insert-file-contents file)
                  (buffer-string)))
          (html (org-web-tools--sanitize-html html))
          ((title . readable) (org-web-tools--eww-readable html))
          (title (org-web-tools--cleanup-title (or title "")))
          (converted (org-web-tools--html-to-org-with-pandoc readable)))
    (with-current-buffer (generate-new-buffer "*html*")
      (org-mode)
      (when (fboundp 'org-indent-mode)
        (org-indent-mode -1))
      ;; Insert article text
      (insert converted)
      ;; Demote in-article headings
      ;; MAYBE: Use `org-paste-subtree' instead of demoting headings ourselves.
      (org-web-tools--demote-headings-below 2)
      ;; Insert headings at top
      (goto-char (point-min))
      (pop-to-buffer (current-buffer)))))

;;;; Overriding the title function of org-web-tools-insert-link-for-url

(cl-defun akirak/org-web-tools--org-link-for-url
    (&optional (url (org-web-tools--get-first-url)))
  "Return Org link to URL using title of HTML page at URL.
If URL is not given, look for first URL in `kill-ring'."
  (cond
   ;; Just show the repository path (USER/REPO) if the URL is a GitHub repository
   ((and (boundp 'akirak/github-https-url-regexp)
         (string-match akirak/github-https-url-regexp url))
    (let ((repo-path (match-string 1 url)))
      (org-make-link-string (concat "https://github.com/" repo-path) repo-path)))
   (t
    (with-timeout (3 (message "Timeout while retrieving %s" url)
                     url)
      (let* ((html (org-web-tools--get-url url))
             (title (org-web-tools--html-title html))
             (link (org-make-link-string url title)))
        link)))))

(advice-add 'org-web-tools--org-link-for-url
            :override #'akirak/org-web-tools--org-link-for-url)

;;;; Overriding the URL picking mechanism

(defun akirak/get-recent-url ()
  ;; TODO: Maybe provide a sensible URL as the default value
  ;; (if (require 'clipurl nil t)
  ;;     (car (clipurl--urls-in-kill-ring))
  ;;   (org-web-tools--get-first-url))
  (clipurl-complete-url "Select a URL to insert: "))

(advice-add 'org-web-tools--get-first-url
            :override 'akirak/get-recent-url)

(provide 'setup-org-web-tools)
