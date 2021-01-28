;; Install packages from nixpkgs.
(straight-use-package '(pdf-tools :type built-in))
(straight-use-package '(org-pdftools :type built-in))

;; pdf-tools blocks startup, so load pdf-loader instead.
;;
;; For details, see https://github.com/politza/pdf-tools#installing
(use-package pdf-loader
  :straight pdf-tools
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
  (pdf-loader-install)
  ;; This is necessary to add a corresponding entry to auto-mode-list.
  (load "pdf-tools-autoloads"))

(use-package org-pdftools
  ;; According to profiling, org-pdftools terribly slows down
  ;; loading of Org files.
  :disabled t
  :after org
  :hook
  (org-mode . org-pdftools-setup-link))

(use-package org-noter
  :after org
  :init
  (setq org-noter-notes-search-path nil)
  :custom
  (org-noter-default-notes-file-names nil)
  (org-noter-always-create-frame nil))

(defun akirak/org-noter-find-document ()
  "Visit NOTER_DOCUMENT of the current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (unless (org-at-heading-p)
      (re-search-forward org-heading-regexp nil t))
    (if-let (doc (org-entry-get nil "NOTER_DOCUMENT"))
        (find-file doc)
      (user-error "Cannot find NOTER_DOCUMENT property in the file"))))

(general-def :keymaps 'org-mode-map :package 'org :prefix akirak/mode-prefix-key
  "nf" #'akirak/org-noter-find-document)

(provide 'setup-referencing)
