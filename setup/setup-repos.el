(use-package git-identity
  :after magit
  :straight (git-identity :host github :repo "akirak/git-identity.el")
  :general
  (:keymaps 'magit-status-mode-map :package 'magit
            "I" #'git-identity-info)
  :custom
  (git-identity-magit-mode t))

(defun akirak/mr-register ()
  (interactive)
  (let* ((root (car-safe (ignore-errors (project-roots (project-current)))))
         (location-ok (string-match-p (rx bol "~/projects/" (+ (not (any "/")))
                                          "/" (+ (not (any "/")))
                                          (?  "/") eol)
                                      (abbreviate-file-name root)))
         (parent (f-parent root))
         (mrconfig (f-join parent ".mrconfig"))
         (has-mrconfig (f-file-p mrconfig)))
    (when (and (or root (user-error "No project root"))
               (or location-ok (user-error "Wrong project location %s" root))
               (or has-mrconfig (user-error "No mrconfig in %s" parent)))
      (let* ((name (f-filename root))
             (remote (or (car-safe (ignore-errors
                                     (process-lines "git" "config" "remote.origin.pushurl")))
                         (car-safe (ignore-errors
                                     (process-lines "git" "config" "remote.origin.url")))))
             (require-explicit-name (not (string-suffix-p (concat "/" name ".git") remote)))
             (case-fold-search nil))
        (with-current-buffer (or (find-buffer-visiting mrconfig)
                                 (find-file-noselect mrconfig))
          (goto-char (point-min))
          (if (re-search-forward (rx bol "[" (eval name) "]") nil t)
              (user-error "Already registered %s in %s" name mrconfig)
            (goto-char (point-max))
            (insert (format "\n[%s]\ncheckout = git clone %s%s\n"
                            name remote
                            (if require-explicit-name
                                (concat " " name)
                              "")))
            (save-buffer)
            (message "Registered %s to %s" name mrconfig)))))))

(provide 'setup-repos)
