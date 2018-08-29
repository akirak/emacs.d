(require 'init-org)
(require 'init-frames)

(dolist (file (directory-files (expand-file-name "local" user-emacs-directory)
                               t "^my-.+\\.el"))
  (condition-case err
      (require (intern (file-name-base file)) file)
    (error (message "%s was not loaded properly: %s" file err))))

(provide 'init-local)
