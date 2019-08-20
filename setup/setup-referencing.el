(use-package pdf-tools
  :straight nil
  :preface
  (autoload 'pdf-annot-minor-mode "pdf-annot")
  (autoload 'pdf-cache-minor-mode "pdf-cache")
  (autoload 'pdf-dev-minor-mode "pdf-dev")
  (autoload 'pdf-history-minor-mode "pdf-history")
  (autoload 'pdf-isearch-minor-mode "pdf-isearch")
  (autoload 'pdf-links-minor-mode "pdf-links")
  (autoload 'pdf-misc-minor-mode "pdf-misc")
  (autoload 'pdf-occur-global-minor-mode "pdf-occur")
  (autoload 'pdf-occur-ibuffer-minor-mode "pdf-occur")
  (autoload 'pdf-outline-minor-mode "pdf-outline")
  (autoload 'pdf-sync-minor-mode "pdf-sync")
  :config
  (pdf-tools-install))

(use-package org-pdftools
  :straight (org-pdftools :host github :repo "fuxialexander/org-pdftools"))

(use-package org-noter
  :after org-starter
  :custom
  (org-noter-default-notes-file-names
   `("noter-notes.org"
     ,(org-starter-locate-file "noter-notes.org" nil t))))

(provide 'setup-referencing)
