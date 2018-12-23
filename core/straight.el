;; This library simply installs straight.el.
;; No further configuration is done.
(unless (featurep 'straight)
  (let ((bootstrap-file (concat user-emacs-directory
                                "straight/repos/straight.el/bootstrap.el"))
        (bootstrap-version 3))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage)))
