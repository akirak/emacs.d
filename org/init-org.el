;;;; Basic configuration
(require 'init-org-base)
(require 'init-org-babel)
(require 'init-org-starter)

;;;; Enhancements to org-mode
(require 'init-org-enhance)
(require 'init-org-edna)
(with-eval-after-load 'org
  (require 'akirak-org-refile-path))

;;;; Convenient tools for editing org files
(require 'init-org-download)
(require 'init-org-web-tools)
(require 'init-org-make-toc)
(require 'init-counsel-org-capture-string)

;;;; Search and navigation
(require 'init-helm-org-rifle)
(require 'init-org-offtime)
(require 'init-counsel-org-clock)

;;;; Additional exporters
(require 'init-org-mind-map)

;;;; Utilities for customization
(require 'init-org-capture)
(require 'init-org-agenda)
(require 'init-org-super-agenda)
(require 'init-org-timeline)

;;;; Set options
(require 'init-org-options)

;;;; Keybindings
(require 'init-org-bindings)
(require 'init-org-agenda-bindings)
(require 'org-refile-hydra)
(require 'org-insert-hydra)

;;;; Specific files
(require 'init-emacs-org)

(akirak/define-frame-workflow "org"
  :key "o"
  :layout '(progn
             (org-starter-load-all-known-files)
             (when (fboundp #'ibuffer-sidebar-show-sidebar)
               (ibuffer-sidebar-show-sidebar)))
  :make-frame '(frame-purpose-make-mode-frame 'org-mode))

(provide 'init-org)
