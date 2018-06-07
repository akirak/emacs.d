(require 'subr-x)

;;;; Bootstrap straight.el package manager
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

;;;; Set up use-package

;; Install use-package using straight.el
(straight-use-package 'use-package)

;; Use straight.el by default in use-package directives
(setq straight-use-package-by-default t)

;;;;; Extend use-package
;; These packages are required in other use-package directives declared in this
;; configuration.

;; Save efforts to configure individual variable files using no-littering
(use-package no-littering
  :init
  (setq no-littering-var-directory
        (expand-file-name ".cache" user-emacs-directory)))

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :init
  (exec-path-from-shell-initialize))

;; Allow :diminish keyword in use-package directives
(require 'init-diminish)
;; Allow use of :general keyword in use-package directives
(require 'init-general)
;; Allow use of :wk keyword in general.el keybinding definitions
(require 'init-which-key)
;; Allow installation of system packages via use-package
(require 'init-system-packages)

(defmacro akirak/define-frame-workflow (name &rest args)
  (declare (indent 1))
  `(with-eval-after-load 'frame-workflow
     (frame-workflow-define-subject ,name ,@args)))

(provide 'init-config)
