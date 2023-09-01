(require 'my/helm/action/github)

(defconst akirak/helm-github-following-source
  (helm-make-source "People I follow on GitHub" 'helm-source-sync
    :candidates (lambda ()
                  (ignore-errors
                    (akirak/github--following)))
    :candidate-transformer
    (-partial #'mapcar (lambda (pairs)
                         (cons (let ((login (alist-get 'login pairs))
                                     (name (alist-get 'name pairs))
                                     (company (alist-get 'company pairs))
                                     (location (alist-get 'location pairs)))
                                 (concat login
                                         (if name
                                             (format " (%s)" name)
                                           "")
                                         (if company
                                             (format " at %s" company)
                                           "")
                                         (if location
                                             (format " in %s" location)
                                           "")))
                               pairs)))
    :action 'akirak/helm-github-user-action))

(provide 'my/helm/source/github)
