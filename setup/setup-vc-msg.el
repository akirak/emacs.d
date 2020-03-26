(use-package vc-msg
  :straight (:host github :repo "akirak/vc-msg" :branch "devel")
  :config/el-patch
  (defun vc-msg-show ()
    "Show commit message of current line.
If Git is used and some text inside the line is selected,
the correct commit which submits the selected text is displayed."
    (interactive)
    (let ((plugin (vc-msg-find-plugin)))
      (setq vc-msg-current-file (funcall vc-msg-get-current-file-function))
      (setq vc-msg-executer (plist-get plugin :execute))
      (setq vc-msg-formatter (plist-get plugin :format))
      (vc-msg-hydra/body)))
  :config
  (pretty-hydra-define vc-msg-hydra
    (:title (concat "vc-msg\n\n"
                    (cl-etypecase vc-msg-commit-info
                      (list
                       (let* ((pad 2)
                              (padstr (make-string pad ?\ ))
                              (width (- (min (window-width) 100) (* pad 2)))
                              (src (funcall vc-msg-formatter vc-msg-commit-info)))
                         (thread-last (split-string src "\n")
                           (mapcar (lambda (s) (seq-partition s width)))
                           (apply #'append)
                           (mapcar (lambda (s) (concat padstr s padstr)))
                           (funcall (lambda (xs) (string-join xs "\n"))))))
                      (string
                       vc-msg-commit-info)))
            :pre (setq vc-msg-commit-info
                       (when vc-msg-current-file
                         (funcall vc-msg-executer
                                  vc-msg-current-file
                                  (funcall vc-msg-get-line-num-function)
                                  (funcall vc-msg-get-version-function))))
            :quit-key ("C-g" "q"))
    ("Copy info"
     (("wa" vc-msg-copy-all "All" :exit t)
      ("wl" vc-msg-copy-link "Commit URL" :exit t)
      ("wb" browse-at-remote-kill "Line URL" :exit t))
     "Visit"
     (("c" vc-msg-show-commit "Commit" :exit t)
      ("l" vc-msg-log "Log" :exit t))
     "Blame"
     (("b" (if magit-blame-mode
               (magit-blame-cycle-style)
             (magit-blame-addition nil))
       "Show")
      ("B" magit-blame-quit "Quit")))))

(provide 'setup-vc-msg)
