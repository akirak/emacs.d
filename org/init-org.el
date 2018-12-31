;;;; Basic configuration
(require 'init-org-starter)

;;;; Enhancements to org-mode
(require 'init-org-enhance)
(require 'init-org-edna)
(require 'init-org-recent-headings)
(with-eval-after-load 'org
  (require 'akirak-org-refile-path))
(use-package org-reverse-datetree
  :straight (org-reverse-datetree :host github
                                  :repo "akirak/org-reverse-datetree"))

;;;; Convenient tools for editing org files
(require 'init-org-download)
(require 'init-org-web-tools)
(require 'init-counsel-org-capture-string)

;;;; Search and navigation
(require 'init-org-offtime)
(require 'init-counsel-org-clock)
(require 'init-counsel-org-bookmark)
(require 'init-calfw-org)

;;;; Additional exporters
(require 'init-org-mind-map)

;;;; Utilities for customization
(require 'init-org-capture)
(require 'init-org-super-agenda)
;; (require 'init-org-timeline)

;;;; Set options
(require 'init-org-options)

;;;; Keybindings
(require 'org-refile-hydra)
(require 'org-insert-hydra)

;;;; Specific files
(require 'init-emacs-org)

(akirak/define-frame-workflow "org"
  :key "o"
  :layout '(progn
             (org-starter-load-all-known-files)
             (when (fboundp #'ibuffer-sidebar-show-sidebar)
               (ibuffer-sidebar-show-sidebar)
               (with-current-buffer (ibuffer-sidebar-buffer (selected-frame))
                 (ibuffer-projectile-set-filter-groups)
                 (ibuffer-update nil))))
  :make-frame '(frame-purpose-make-mode-frame 'org-mode))

(provide 'init-org)
