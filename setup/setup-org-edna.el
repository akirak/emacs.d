(use-package org-edna
  :after org
  :straight (org-edna :host github :repo "akirak/org-edna" :branch "edit")
  :init
  (defun akirak/org-edna-edit-setup-company ()
    (setq company-backends '(company-capf))
    (company-mode 1))
  (org-edna-load)
  :config
  (defun akirak/org-edna--add-id (id old)
    (if (string-match (rx "ids("
                          (group (+ (any digit "-a-f"))
                                 (*? (and (+ space) (+ (any digit "-a-f")))))
                          ")")
                      old)
        (let ((new-ids (format "ids(%s %s)" (match-string 1 old) id)))
          (s-replace (match-string 0 old) new-ids old))
      (format "%s ids(%s)" old id)))
  (defun akirak/org-edna-add-id-blocker (id)
    (cond
     ((derived-mode-p 'org-mode)
      (let* ((old (org-entry-get nil "BLOCKER")))
        (org-set-property "BLOCKER"
                          (if old
                              (akirak/org-edna--add-id id old)
                            (format "ids(%s)" id)))))
     ((derived-mode-p 'org-agenda-mode)
      (let* ((buffer-orig (buffer-name))
             (marker (or (org-get-at-bol 'org-hd-marker)
                         (org-agenda-error)))
             (buffer (marker-buffer marker))
             (pos (marker-position marker)))
        (with-current-buffer buffer
          (org-with-wide-buffer
           (goto-char marker)
           (let ((old (org-entry-get nil "BLOCKER")))
             (org-set-property "BLOCKER"
                               (if old
                                   (akirak/org-edna--add-id id old)
                                 (format "ids(%s)" id))))))
        (org-agenda-redo)))
     (t (user-error "Unsupported mode"))))
  (defun akirak/avy-add-org-edna-id-blocker ()
    (interactive)
    (unless (derived-mode-p 'org-mode 'org-agenda-mode)
      (user-error "Not in org-mode or org-agenda-mode"))
    (let ((id (save-selected-window (org-starter-utils-avy-id))))
      (akirak/org-edna-add-id-blocker id)))
  (defun akirak/helm-org-rifle-add-edna-blocker-with-id (candidate)
    (unless (derived-mode-p 'org-mode 'org-agenda-mode)
      (user-error "Not in org-mode or org-agenda-mode"))
    (let ((id (with-current-buffer (car candidate)
                (org-with-wide-buffer
                 (goto-char (cdr candidate))
                 (org-id-get-create t)))))
      (akirak/org-edna-add-id-blocker id)))
  :hook
  (org-edna-edit-mode . akirak/org-edna-edit-setup-company))

(provide 'setup-org-edna)
