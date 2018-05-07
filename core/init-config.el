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

;; Use diminish to reduce clutters from the modeline
(use-package diminish
  :init
  (diminish 'auto-revert-mode)
  (diminish 'outline-minor-mode)
  (diminish 'flyspell-mode))

;; Allow you to define keybindings more concisely
(require 'init-general)

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

(provide 'init-config)
