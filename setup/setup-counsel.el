(use-package counsel
  ;; :diminish counsel-mode
  :config
  (counsel-mode 1) ; Remap built-in functions with counsel equivalents
  (ivy-add-actions #'counsel-find-library
                   '(("l" load-library "load")))
  (cl-loop for (command find-other-window)
           in '((counsel-describe-function find-function-other-window)
                (counsel-describe-variable find-variable-other-window)
                (counsel-M-x find-function-other-window))
           do (ivy-add-actions command
                               `(("j" ,(-compose find-other-window 'intern)
                                  "definition in other window"))))
  (ivy-add-actions #'counsel-rg
                   `(("j" ,(-partial #'akirak/counsel-git-grep-action-with-find-file
                                     #'find-file-other-window)
                      "other window")
                     ("f" ,(-partial #'akirak/counsel-git-grep-action-with-find-file
                                     #'find-file-other-frame)
                      "other frame")))
  (global-set-key [remap recentf-open-files] 'counsel-recentf)
  (global-set-key [remap insert-char] 'counsel-unicode-char)
  :custom
  ;; Let counsel-find-file-at-point choose the file under cursor
  ;; https://www.reddit.com/r/emacs/comments/7wjyhy/emacs_tip_findfileatpoint/du1xlbg/
  (counsel-find-file-at-point t))

(defun akirak/ad-after-counsel-org-goto-action (_x)
  (org-show-entry))
(advice-add 'counsel-org-goto-action :after
            'akirak/ad-after-counsel-org-goto-action)

(defun akirak/counsel-git-grep-action-with-find-file (find-file-func x)
  "Go to occurrence X in current Git repository."
  (when (string-match "\\`\\(.*?\\):\\([0-9]+\\):\\(.*\\)\\'" x)
    (let ((file-name (match-string-no-properties 1 x))
          (line-number (match-string-no-properties 2 x)))
      (funcall find-file-func (expand-file-name
                               file-name
                               (ivy-state-directory ivy-last)))
      (goto-char (point-min))
      (forward-line (1- (string-to-number line-number)))
      (re-search-forward (ivy--regex ivy-text t) (line-end-position) t)
      (swiper--ensure-visible)
      (run-hooks 'counsel-grep-post-action-hook)
      (unless (eq ivy-exit 'done)
        (swiper--cleanup)
        (swiper--add-overlays (ivy--regex ivy-text))))))

(provide 'setup-counsel)
