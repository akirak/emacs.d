(use-package org-web-tools)

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
    (let* ((html (org-web-tools--get-url url))
	   (title (org-web-tools--html-title html))
	   (link (org-make-link-string url title)))
      link))))

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
