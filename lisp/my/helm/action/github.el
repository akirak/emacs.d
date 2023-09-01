(defconst akirak/helm-github-user-action
  (helm-make-actions
   ;; "Browse repositories"
   ;; (-compose #'akirak/browse-github-user-repos (-partial #'alist-get 'login))
   "Visit the profile on GitHub"
   (lambda (pairs)
     (let ((login (alist-get 'login pairs)))
       (akirak/browse-url (format "https://github.com/%s" login))))
   "Visit the home page, if any"
   (lambda (pairs)
     (if-let (websiteUrl (alist-get 'websiteUrl pairs))
         (akirak/browse-url websiteUrl)
       (user-error "No website")))
   "Visit the twitter account"
   (lambda (pairs)
     (if-let (twitter (alist-get 'twitterUsername pairs))
         (akirak/browse-url (format "https://twitter.com/%s" twitter))
       (user-error "No twitter")))))

(provide 'my/helm/action/github)
