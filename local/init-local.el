(require 'init-org)
(require 'init-frames)

(dolist (file (directory-files (expand-file-name "local" user-emacs-directory)
                               t "^my-.+\\.el"))
  (require (intern (file-name-base file)) file t))

(provide 'init-local)
