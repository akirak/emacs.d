;; -*- lexical-binding: t; -*-

(defconst akirak/helm-git-url-dummy-action
  (helm-make-actions
   "Clone to the default location"
   (-compose #'akirak/remote-git-repo-clone-default
             #'akirak/parse-git-url)
   "Search starred repositories on GitHub"
   #'akirak/github-search-starred-repos
   "Search repository on GitHub"
   #'akirak/github-search-repository
   "Search code on GitHub"
   #'akirak/github-search-code))

(provide 'my/helm/action/giturl)
