(use-package org-reverse-datetree)

(use-package org-super-agenda
  :after org-agenda
  :config
  ;; Basically stolen from org-super-agenda.el
  (eval-when-compile
    (org-super-agenda--def-auto-group ts-desc
      "the date of their latest timestamp anywhere in the entry (formatted according to `org-super-agenda-date-format', which see)"
      :keyword :auto-ts-desc
      :key-form (org-super-agenda--when-with-marker-buffer (org-super-agenda--get-marker item)
                  (let* ((limit (org-entry-end-position))
                         (latest-ts (->> (cl-loop for next-ts =
                                                  (when (re-search-forward org-element--timestamp-regexp limit t)
                                                    (ts-parse-org (match-string 1)))
                                                  while next-ts
                                                  collect next-ts)
                                         (-sort #'ts>)
                                         car)))
                    (when latest-ts
                      (propertize (ts-format org-super-agenda-date-format latest-ts)
                                  'org-super-agenda-ts latest-ts))))
      :key-sort-fn (lambda (a b)
                     ;; This part has been changed from `ts<' to `ts>'.
                     (ts> (get-text-property 0 'org-super-agenda-ts a)
                          (get-text-property 0 'org-super-agenda-ts b)))))
  (org-super-agenda-mode 1))

(use-package org-ql-search
  :straight org-ql)

(use-package org-ql-view
  :straight org-ql)

(use-package helm-org-ql
  :straight org-ql)

(use-package org-multi-wiki
  :straight (org-multi-wiki :host github :repo "akirak/org-multi-wiki")
  :config
  (defun akirak/org-multi-wiki-entry-template-fn (heading)
    (concat "* " heading "\n:PROPERTIES:\n"
            ":CREATED_TIME: " (org-format-time-string (org-time-stamp-format 'long 'inactive))
            "\n:END:\n"))
  :custom
  (org-multi-wiki-entry-template-fn #'akirak/org-multi-wiki-entry-template-fn))

(use-package helm-org-multi-wiki
  :straight org-multi-wiki)

(unless (bound-and-true-p org-starter-path)
  (setq org-starter-path `(,(abbreviate-file-name
                             (expand-file-name
                              "org-starter"
                              no-littering-etc-directory)))))

(use-package org-starter
  :straight (org-starter :host github :repo "akirak/org-starter"
                         :branch "devel")
  :config
  (org-starter-mode 1)
  (org-starter-def "~/.emacs.d/main.org"
    :key "m"
    :refile (:maxlevel . 5))
  (org-starter-def "~/home.nix/README.org"
    :key "n"
    :refile (:maxlevel . 3))
  (general-add-hook 'org-starter-extra-find-file-map
                    '((";" org-starter-find-config-file "config"))
                    t)
  (general-add-hook 'org-starter-extra-alternative-find-file-map
                    '((";" org-starter-swiper-config-files "config"))
                    t)
  (general-add-hook 'org-starter-extra-refile-map
                    '(("'" avy-org-refile-as-child "avy")
                      ("?" akirak/org-refile-same-buffer "same buffer"))
                    t)
  (defun akirak/helm-org-ql-known-files ()
    (interactive)
    (helm-org-ql org-starter-known-files))
  :custom
  (org-starter-load-config-files t)
  (org-starter-require-file-by-default nil)
  (org-starter-exclude-from-recentf '(known-files path))
  (org-starter-alternative-find-function #'helm-org-ql)
  (org-starter-find-file-visit-window t)
  (org-starter-override-agenda-window-setup 'other-window)
  (org-starter-enable-local-variables :all))

(use-package org-starter-swiper
  :straight org-starter)

(use-package org-starter-extras
  :straight org-starter)

;;;; Extra keybindings
(akirak/bind-search
  "M-a" #'helm-org-ql-agenda-files
  "M-k" #'akirak/helm-org-ql-known-files
  "M-o" #'org-starter-alternative-find-file-by-key
  "M-w" #'helm-org-multi-wiki-all)
(akirak/bind-jump
  "M-o" #'org-starter-find-file-by-key)
(akirak/bind-mode :keymaps 'org-mode-map :package 'org
  "r" #'org-starter-refile-by-key)

(provide 'setup-org-starter)
