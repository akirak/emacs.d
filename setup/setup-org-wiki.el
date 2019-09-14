(use-package plain-org-wiki
  :straight (plain-org-wiki :host github :repo "abo-abo/plain-org-wiki")
  :config/el-patch
  (el-patch-defun plain-org-wiki-find-file (x)
    "Open X as a file with org extension in `plain-org-wiki-directory'."
    (when (consp x)
      (setq x (cdr x)))
    (with-ivy-window
      (if (file-exists-p x)
          (find-file x)
        (if (string-match "org$" x)
            (find-file
             (expand-file-name x plain-org-wiki-directory))
          (find-file
           (expand-file-name
            (el-patch-swap (format "%s.org" x)
                           (format "%s.org" (akirak/escape-wiki-file-name x)))
            plain-org-wiki-directory))))))
  :config
  (defun akirak/escape-wiki-file-name (name)
    (s-replace " " "_" name))
  (defun akirak/unescape-wiki-file-name (filename)
    (s-replace "_" " " filename)))

(provide 'setup-org-wiki)
