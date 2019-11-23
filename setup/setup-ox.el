;;; Miscellaneous ox backends

;; Some exporters, e.g. =ox-hugo=, depend on =ox-org=, and it is
;; tedious to add =(require 'ox-org)= to all of their configurations,
;; so I will load it immediately after =ox= is loaded.

(use-package ox
  :after org
  :straight nil
  :config
  ;; Workaround for preventing a loading error in some exporter packages
  (require 'ox-org))

(use-package org-mind-map
  :after ox)

;; Generate README from Emacs Lisp.
(use-package ox-gfm
  :after ox)

;; Exporting to Markdown used in Qiita (a Japanese blog site).
(use-package ox-qmd
  :after ox)

(provide 'setup-ox)
