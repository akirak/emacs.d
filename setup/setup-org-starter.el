(autoload 'helm-org-rifle-files "helm-org-rifle")

(defun helm-org-rifle-known-files ()
  (interactive)
  (helm-org-rifle-files org-starter-known-files))

(use-package org-reverse-datetree)

(use-package org-super-agenda
  :after org-agenda
  :config
  (org-super-agenda-mode 1))

(use-package org-ql-agenda
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
  (general-add-hook 'org-starter-extra-alternative-find-file-map
                    '((";" org-starter-swiper-config-files "config"))
                    t)
  (general-add-hook 'org-starter-extra-refile-map
                    '(("'" avy-org-refile-as-child "avy")
                      ("?" akirak/org-refile-same-buffer "same buffer"))
                    t)
  (org-starter-def-capture "e" "Personal Emacs configuration"
    plain (function (lambda ()
                      (org-starter-find-config-file)
                      (goto-char (point-max))))
    "%i"
    :unnarrowed t)
  :custom
  (org-starter-load-config-files t)
  (org-starter-require-file-by-default nil)
  (org-starter-exclude-from-recentf '(known-files path))
  (org-starter-alternative-find-file-command #'helm-org-rifle-files)
  (org-starter-find-file-visit-window t)
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
