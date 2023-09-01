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

(cl-defun akirak/helm-org-narrow-to-subtree-action (marker &key full-width)
  (require 'my/org/posframe)
  (let ((buffer (akirak/org-subtree-temporary-posframe
                 marker
                 akirak/helm-org-posframe-temporary-buffer)))
    (posframe-show buffer
                   :poshandler
                   (if full-width
                       #'posframe-poshandler-frame-bottom-left-corner
                     #'posframe-poshandler-frame-bottom-right-corner)
                   :respect-header-line t
                   :internal-border-color "white"
                   :internal-border-width 2
                   :width (/ (frame-width) (if full-width 1 2))
                   :height 30)))

(cl-defun akirak/helm-execute-sh-src-block-action (marker func
                                                          &key project)
  (require 'my/org/block)
  (akirak/helm-org-narrow-to-subtree-action marker :full-width t)
  (unwind-protect
      (let* ((default-directory (or (and project
                                         (akirak/project-root default-directory))
                                    default-directory))
             (command (read-string (format "Execute command in %s using %s: "
                                           (abbreviate-file-name default-directory)
                                           func)
                                   (akirak/helm-org-first-src-block-body marker))))
        (funcall func command))
    (posframe-delete akirak/helm-org-posframe-temporary-buffer)))

(defconst akirak/helm-org-marker-sh-block-action-list
  '(("compile at project root"
     . (lambda (marker)
         (akirak/helm-execute-sh-src-block-action marker #'compile :project t)))
    ("compile at working directory"
     . (lambda (marker)
         (akirak/helm-execute-sh-src-block-action marker #'compile)))
    ("eshell command at project root"
     . (lambda (marker)
         (akirak/helm-execute-sh-src-block-action marker #'eshell-command :project t)))
    ("eshell command at working directory"
     . (lambda (marker)
         (akirak/helm-execute-sh-src-block-action marker #'eshell-command)))
    ("Show the whole entry" . helm-org-ql-show-marker-indirect)))

(provide 'my/helm/action/org-marker)
