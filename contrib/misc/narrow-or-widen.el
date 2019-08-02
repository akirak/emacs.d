;; Based on http://endlessparentheses.com/emacs-narrow-or-widen-dwim.html
;;;###autoload
(defun akirak/narrow-or-widen-dwim (arg)
  "Widen if buffer is narrowed, narrow-dwim otherwise.
Dwim means: region, org-src-block, org-subtree, or
defun, whichever applies first. Narrowing to
org-src-block actually calls `org-edit-src-code'.

When a universal prefix argument is given, create an indirect buffer
to the corresponding area instead of narrowing to it. If the current buffer
is an indirect buffer, this command doesn't do anything."
  (interactive "P")
  (declare (interactive-only))
  (cond ((and arg (buffer-base-buffer)) nil)
        ((and arg (region-active-p))
         (let ((start (region-beginning))
               (end (region-end))
               (name (read-string "Name of the indirect buffer to create: ")))
           (clone-indirect-buffer name t)
           (narrow-to-region start end)
           (set-mark nil)))
        ((and arg (derived-mode-p 'org-mode))
         (org-tree-to-indirect-buffer))
        (arg
         (let ((start (save-excursion
                        (beginning-of-defun)
                        (point)))
               (end (save-excursion
                      (end-of-defun)
                      (point)))
               (name (read-string "Name of the indirect buffer to create: "
                                  (akirak/default-name-for-indirect-buffer))))
           (clone-indirect-buffer name t)
           (narrow-to-region start end)
           (set-mark nil)))
        ((buffer-narrowed-p) (widen))
        ((region-active-p)
         (narrow-to-region (region-beginning)
                           (region-end)))
        ((derived-mode-p 'org-mode)
         ;; `org-edit-src-code' is not a real narrowing
         ;; command. Remove this first conditional if
         ;; you don't want it.
         (cond ((ignore-errors (org-edit-src-code) t)
                (delete-other-windows))
               ((ignore-errors (org-narrow-to-block) t))
               (t (org-narrow-to-subtree))))
        ((derived-mode-p 'latex-mode)
         (LaTeX-narrow-to-environment))
        (t (narrow-to-defun))))

(defun akirak/default-name-for-indirect-buffer ()
  (let ((local-name (when (bound-and-true-p which-function-mode)
                      (which-function)))
        (file-name (buffer-file-name)))
    (cond
     ((and local-name file-name)
      (format "%s in %s" local-name (file-name-nondirectory file-name)))
     (t local-name))))

(provide 'narrow-or-widen)
