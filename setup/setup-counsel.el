(use-package counsel
  ;; :diminish counsel-mode
  :config
  ;; counsel-rg may fail in a direnv + shell.nix + lorri environment,
  ;; so I included the absolute path of rg in the command line.
  (setq counsel-rg-base-command
        (replace-regexp-in-string (rx bol "rg ")
                                  (let ((exec-path (default-value 'exec-path)))
                                    (concat (executable-find "rg")
                                            " "))
                                  counsel-rg-base-command))
  (counsel-mode 1) ; Remap built-in functions with counsel equivalents
  (ivy-add-actions #'counsel-find-library
                   '(("l" load-library "load")
                     ("g" akirak/magit-status-of-library "git repo")
                     ("d" akirak/dired-of-library "dired")))
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
  (ivy-add-actions #'counsel-find-file
                   '(("gs" magit-status "magit-status")))
  (global-set-key [remap recentf-open-files] 'counsel-recentf)
  (global-set-key [remap insert-char] 'counsel-unicode-char)
  :general
  (:keymaps 'counsel-find-file-map
            "C-c g" #'akirak/counsel-find-file-magit-status)
  :custom
  ;; Let counsel-find-file-at-point choose the file under cursor
  ;; https://www.reddit.com/r/emacs/comments/7wjyhy/emacs_tip_findfileatpoint/du1xlbg/
  (counsel-find-file-at-point t))

(defun akirak/counsel-find-file-magit-status ()
  (interactive)
  (ivy-exit-with-action
   #'magit-status))

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

(defun akirak/magit-status-of-library (x)
  (if-let ((path (find-library-name x))
           (repo (locate-dominating-file (file-truename path) ".git")))
      (magit-status repo)
    (user-error "Cannot find library or its directory %s" x)))

(defun akirak/dired-of-library (x)
  (if-let ((path (find-library-name x))
           (truename (file-truename path)))
      (dired-jump nil (cond
                       ((equal truename path) path)
                       ((yes-or-no-p "Follow symbolic link? ")
                        truename)
                       (t path)))
    (user-error "Cannot find library or its directory %s" x)))

(use-package counsel-projectile
  :after (projectile counsel)
  :init
  (counsel-projectile-mode 1)
  :config
  (defun counsel-projectile-other-frame-action (name)
    "Switch to buffer or find file named NAME."
    (if (member name counsel-projectile--buffers)
        (switch-to-buffer-other-frame name)
      (find-file-other-frame (projectile-expand-root name))
      (run-hooks 'projectile-find-file-hook)))
  (ivy-add-actions 'counsel-projectile
                   '(("f" counsel-projectile-other-frame-action "frame")))
  (ivy-add-actions 'counsel-projectile-switch-project
                   '(("gs" magit-status "magit-status"))))

(use-package counsel-tramp
  :commands (counsel-tramp))

(use-package counsel-world-clock)

(provide 'setup-counsel)
