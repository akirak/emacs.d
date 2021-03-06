(use-package org-make-toc
  :after (org)
  :commands (org-make-toc org-make-toc-mode)
  :config
  (defun akirak/org-insert-toc-for-top-level ()
    (interactive)
    (unless (derived-mode-p 'org-mode)
      (user-error "Not in org-mode"))
    (org-with-wide-buffer
     (or (re-search-backward (rx bol "* ") nil t)
         (re-search-forward (rx bol "* ") nil t))
     (org-narrow-to-subtree)
     (when (org-find-property "TOC")
       (user-error "Already has a TOC"))
     (let ((heading (concat (make-string (org-get-valid-level 2) ?\*)
                            " ")))
       (if (re-search-forward (concat "^" (regexp-quote heading))
                              nil t)
           (beginning-of-line 1)
         (org-end-of-subtree)
         (unless (= 0 (car (posn-col-row (posn-at-point))))
           (insert "\n")))
       (insert heading "Table of contents\n")
       (beginning-of-line 0)
       (org-set-property "TOC" "siblings")))
    (add-file-local-variable 'before-save-hook 'org-make-toc)
    (save-buffer)
    (revert-buffer)))

(provide 'setup-org-make-toc)
