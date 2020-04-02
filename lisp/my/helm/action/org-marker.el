(defun akirak/helm-yankpad-expand-src-action (marker)
  (let* ((buffer (marker-buffer marker))
         (org-indirect-buffer-display 'other-window)
         (snippets
          (save-selected-window
            (with-current-buffer buffer
              (org-with-wide-buffer
               (goto-char marker)
               (prog1
                   (when (member "src" (org-get-tags))
                     (yankpad-snippets-at-point))
                 (org-tree-to-indirect-buffer)))))))
    (when (= 1 (length snippets))
      (yankpad--run-snippet (car snippets)))))

(defun akirak/helm-org-narrow-to-subtree-action (marker)
  (let ((buffer (marker-buffer marker)))
    (display-buffer buffer)
    (with-current-buffer buffer
      (widen)
      (goto-char marker)
      (org-narrow-to-subtree))))

(cl-defun akirak/helm-execute-sh-src-block-action (marker func
                                                          &key project)
  (require 'my/org/block)
  ;; TODO: Display the entry before running the command
  (let* ((default-directory (or (and project
                                     (akirak/project-root default-directory))
                                default-directory))
         (command (read-string (format "Execute command in %s using %s: "
                                       (abbreviate-file-name default-directory)
                                       func)
                               (akirak/helm-org-first-src-block-body marker))))
    (funcall func command)))

(provide 'my/helm/action/org-marker)
