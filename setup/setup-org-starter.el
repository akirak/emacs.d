(autoload 'helm-org-rifle-files "helm-org-rifle")

(defun helm-org-rifle-known-files ()
  (interactive)
  (helm-org-rifle-files org-starter-known-files))

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

(unless (bound-and-true-p org-starter-path)
  (setq org-starter-path `(,(abbreviate-file-name
                             (expand-file-name
                              "org-starter"
                              no-littering-etc-directory)))))

(use-package org-starter
  :config
  (org-starter-mode 1)
  (org-starter-def "~/.emacs.d/main.org"
    :key "m"
    :refile (:maxlevel . 5))
  (org-starter-def "~/home.nix/README.org"
    :key "n"
    :refile (:maxlevel . 3))
  (general-add-hook 'org-starter-extra-find-file-map
                    '((";" org-starter-find-config-file "config")
                      ("w" org-plain-wiki "wiki"))
                    t)
  (general-add-hook 'org-starter-extra-alternative-find-file-map
                    '((";" org-starter-swiper-config-files "config")
                      ("w" helm-org-rifle-wiki "wiki/writing"))
                    t)
  (general-add-hook 'org-starter-extra-refile-map
                    '(("'" avy-org-refile-as-child "avy")
                      ("?" akirak/org-refile-same-buffer "same buffer"))
                    t)
  :custom
  (org-starter-load-config-files t)
  (org-starter-require-file-by-default nil)
  (org-starter-exclude-from-recentf '(known-files path))
  (org-starter-alternative-find-file-command #'helm-org-rifle-files)
  (org-starter-find-file-visit-window t)
  (org-starter-override-agenda-window-setup 'other-window)
  (org-starter-enable-local-variables :all))

(use-package org-starter-swiper)

(use-package org-starter-extras
  :straight (org-starter-extras :host github :repo "akirak/org-starter"
                                :files ("org-starter-extras.el")))

;;;; Extra keybindings
(akirak/bind-user
  "j" #'org-starter-alternative-find-file-by-key)

(akirak/bind-mode :keymaps 'org-mode-map :package 'org
  "r" #'org-starter-refile-by-key)

(provide 'setup-org-starter)
