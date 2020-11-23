(require 'my/helm/source/dir)
(require 'my/helm/action/dir)
(require 'my/gitmodule/enum)

(defclass akirak/helm-source-gitmodule (helm-source-sync)
  ((action :initform 'akirak/helm-gitmodule-actions-1)))

(defconst akirak/helm-toplevel-repos-submodules-source
  (helm-make-source "Git submodules of top-level repositories in ~"
      akirak/helm-source-gitmodule
    :candidates (lambda ()
                  (-map (lambda (alist)
                          (cons (f-short (alist-get 'location alist))
                                alist))
                        (akirak/toplevel-repos-submodules)))))

(provide 'my/helm/source/gitmodule)
