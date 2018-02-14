;;; ak-config.el --- A configuration infrastructure for Emacs

;;;; Custom variables and functions for configuration

;;;;; Personalisation

;; This configuration repository contains some parts that are applicable only to
;; the author (Akira Komamura). This is done by checking the e-mail address
;; in the Git config:

(require 'subr-x)

(defvar akirak/personalized
  ;; Use the e-mail address to check the user
  (string-equal (string-trim-right (shell-command-to-string
                                    "git config --global user.email") )
                "akira.komamura@gmail.com")
  "Non-nil if the configuration should be personalized.")

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

;; Use diminish to reduce clutters from the modeline
(use-package diminish
  :config
  (diminish 'auto-revert-mode))

;; Allow you to define keybindings more concisely
(use-package general)

;; Show keybindings
(use-package which-key
  :diminish which-key-mode
  :init
  (which-key-mode)
  :config
  (which-key-setup-side-window-bottom)  ; Display a popup window at bottom
  ;; Remove 'akirak/' prefix from descriptions
  (add-to-list 'which-key-replacement-alist
               `((nil . "akirak/") .
                 (lambda (kb)
                   (cons (car kb)
                         (string-remove-prefix "akirak/" (cdr kb)))))))

(provide 'ak-config)
