(use-package org-starter
  :straight (org-starter :host github :repo "akirak/org-starter")
  :functions (org-starter-define-directory org-starter-define-file)
  :custom
  (org-starter-exclude-from-recentf '(known-files path))
  (org-starter-alternative-find-function #'helm-org-rifle-files)
  (org-starter-extra-find-file-map
   '(("j" (lambda () (interactive) (counsel-ag nil "~/personal/org-journal")) "search journal")))
  (org-starter-extra-refile-map
   '(("j" org-refile-to-journal "journal")
     ("'" avy-org-refile-as-child "avy")
     ("?" org-refile-same-buffer "in-buffer")
     ("o" org-refile-other-window-files "other window buffers")
     ("@" (lambda () (interactive) (org-refile 2)) "clock"))))

(use-package helm-org-starter
  :straight org-starter
  :commands (helm-org-starter)
  :defines (helm-org-starter-known-file-source))

(use-package counsel-org-starter
  :straight org-starter
  :commands (counsel-org-starter counsel-org-starter-known-file)
  :config
  (ivy-add-actions 'counsel-org-starter
                   '(("r"
                      (lambda (file)
                        (helm-org-rifle-files (org-starter-locate-file file nil t)))
                      "rifle"))))

(provide 'init-org-starter)
