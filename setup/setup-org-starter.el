(autoload 'helm-org-rifle-files "helm-org-rifle")

(defun helm-org-rifle-known-files ()
  (interactive)
  (helm-org-rifle-files org-starter-known-files))

(use-package org-reverse-datetree)

(cl-defmacro akirak/def-org-reverse-datetree-refile (file
                                                     &rest args
                                                     &key key prefer
                                                     &allow-other-keys)
  "Define a refile function as well as a keybinding."
  (declare (indent 1))
  (let* ((basename (file-name-base file))
         (name (intern (concat "akirak/org-refile-to-" basename))))
    `(defun ,name (arg)
       (interactive "P")
       (org-reverse-datetree-refile-to-file
        (org-starter-locate-file ,file nil t) nil
        :prefer ,(or prefer '("CREATED_TIME" "CREATED_AT" "CLOSED"))
        ,@args))
    `(when (quote ,key)
       (add-to-list 'org-starter-extra-refile-map
                    '(,key ,name ,basename)))))

(unless (bound-and-true-p org-starter-path)
  (setq org-starter-path `(,(abbreviate-file-name
                             (expand-file-name
                              "org-starter"
                              no-littering-etc-directory)))))

(use-package org-starter
  :straight (org-starter :host github :repo "akirak/org-starter")
  :config
  (org-starter-mode 1)
  (require 'akirak/org-todo)
  (org-starter-def "~/.emacs.d/main.org"
    :key "m"
    :refile (:maxlevel . 5))
  (require 'akirak/org-cpb)
  (require 'akirak/org-clock-capture)
  (require 'akirak/org-task-capture)
  (org-starter-def-capture "g" "Generic entry in the inbox (with %i as title)"
    entry (file "scratch.org")
    "* %i%?
:PROPERTIES:
:CREATED_TIME: %U
:END:
"
    :clock-in t :clock-resume t :empty-lines 1)
  (general-add-hook 'org-starter-extra-alternative-find-file-map
                    '((";" org-starter-swiper-config-files "config"))
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
  (org-starter-enable-local-variables :all))

;;;; Extra keybindings
(akirak/bind-user
  "j" #'org-starter-alternative-find-file-by-key)

(akirak/bind-mode :keymaps 'org-mode-map :package 'org
  "r" #'org-starter-refile-by-key)

(provide 'setup-org-starter)
